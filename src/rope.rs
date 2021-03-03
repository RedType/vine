use std::{
	collections::VecDeque,
	ops::{Bound, RangeBounds},
	rc::Rc,
};
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug)]
pub enum Rope {
	Branch {
		weight: usize,
		depth: u32,
		balanced: bool,
		left: Option<Rc<Rope>>,
		right: Option<Rc<Rope>>,
	},
	Leaf {
		weight: usize,
		text: String,
	},
}

impl Rope {
	const LEAF_UNIT_COUNT: usize = 32;

	pub fn from_str(s: &str) -> Rc<Self> {
		// assemble leaves
		let mut gs = s.graphemes(true).peekable();
		let mut leaves = Vec::new();
		// split input string into chunks
		while let Some(_) = gs.peek() {
			let mut length = Self::LEAF_UNIT_COUNT;
			let mut text = String::new();
			for i in 0..Self::LEAF_UNIT_COUNT {
				if let Some(g) = gs.next() {
					text.push_str(g);
				} else {
					length = i;
					break;
				}
			}
			// since this string will never be modified again...
			text.shrink_to_fit();
			leaves.push(Rc::new(Self::Leaf { weight: length, text }));
		}
		Self::from_leaves(&leaves)
	}

	fn from_leaves(leaves: &[Rc<Self>]) -> Rc<Self> {
		match leaves.len() {
			0 => panic!("Attempted to create a rope from 0 leaves"),
			1 => Rc::clone(&leaves[0]),
			n => {
				let half = n / 2;
				let mut root = Self::concat(
					&Self::from_leaves(&leaves[0..half]),
					&Self::from_leaves(&leaves[half..n]),
				);
				// because we're building it for the first time
				if let Self::Branch { balanced, .. } = Rc::get_mut(&mut root).unwrap() {
					*balanced = true;
				}
				root
			},
		}
	}

	pub fn concat(left: &Rc<Self>, right: &Rc<Self>) -> Rc<Self> {
		Rc::new(Self::Branch {
			weight: left.weight(),
			depth: left.depth().max(right.depth()) + 1,
			balanced: false,
			left: Some(Rc::clone(left)),
			right: Some(Rc::clone(right)),
		})
	}

	/// Traverses a rope in order, calling the provided function on each node.
	pub fn inorder<F: Fn(&Self)>(&self, func: &F) {
		match self {
			Self::Branch { left, right, .. } => {
				if let Some(l) = left { l.inorder(func); }
				func(self);
				if let Some(r) = right { r.inorder(func); }
			},
			Self::Leaf { .. } => { func(self); },
		}
	}

	pub fn range<'a, B>(&'a self, range: B) -> RopeIterator<'a>
	where
		B: RangeBounds<usize>,
	{
		use Bound::*;
		let lo = match range.start_bound() {
			Included(i) => *i,
			Excluded(i) => *i + 1, // can ranges even give this as a start bound??
			Unbounded => 0,
		};
		let hi = match range.end_bound() {
			Included(i) => *i + 1,
			Excluded(i) => *i,
			Unbounded => usize::MAX,
		};
		let mut queue = VecDeque::new();
		self.range_helper(&mut queue, lo, hi);
		queue.shrink_to_fit();
		RopeIterator(queue)
	}

	fn range_helper<'a, 'b>(&'a self, queue: &'b mut VecDeque<&'a str>, lo: usize, hi: usize) {
		match *self {
			Rope::Branch { ref left, ref right, weight, .. } => {
				if let Some(ref l) = left {
					if lo < weight {
						l.range_helper(queue, lo, hi);
					}
				}
				if let Some(ref r) = right {
					if weight < hi {
						// subtract but don't go below zero
						let rh_lo = lo.checked_sub(weight).unwrap_or(0);
						let rh_hi = hi.checked_sub(weight).unwrap_or(0);
						r.range_helper(queue, rh_lo, rh_hi);
					}
				}
			},
			Rope::Leaf { ref text, .. } => text.graphemes(true)
				.take(hi)
				.skip(lo)
				.for_each(|g| queue.push_back(g)),
		}
	}

	pub fn depth(&self) -> u32 {
		match self {
			Self::Leaf { .. } => 1,
			Self::Branch { depth, .. } => *depth,
		}
	}

	pub fn weight(&self) -> usize {
		match self {
			Self::Leaf { weight, .. } => *weight,
			Self::Branch { weight, .. } => *weight,
		}
	}
}

pub struct RopeIterator<'a>(VecDeque<&'a str>);

impl<'a> Iterator for RopeIterator<'a> {
	type Item = &'a str;

	fn next(&mut self) -> Option<Self::Item> {
		self.0.pop_front()
	}
}

#[test]
fn builds_without_crashing() {
	let corpus = std::iter::repeat("a").take(100).collect::<String>();
	let _ = Rope::from_str(&corpus);
}

#[test]
fn iterates_full_range() {
	let corpus = std::iter::repeat("a").take(100).collect::<String>();
	let root = Rope::from_str(&corpus);
	let mut corpus2 = String::new();
	root.range(..).for_each(|ch| corpus2.push_str(ch));
	assert_eq!(corpus, corpus2);
}

#[test]
fn iterates_partial_range() {
	let corpus = std::iter::repeat("a").take(100).collect::<String>();
	let root = Rope::from_str(&corpus);
	let mut corpus2 = String::new();
	root.range(32..98).for_each(|ch| corpus2.push_str(ch));
	assert_eq!(corpus2.len(), 98 - 32);
}
