#![allow(dead_code, unused_variables, unused_mut)]

use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt;
use std::fmt::Write;
use std::io;
use std::ops::{Deref, DerefMut};

use rand::{RngCore, SeedableRng};
use rand_chacha::ChaCha8Rng;

const MIN_NUMERAL_CARD_RANK: usize = 2;
const MAX_NUMERAL_CARD_RANK: usize = 10;
const JACK_RANK: usize = MAX_NUMERAL_CARD_RANK + 1;
const QUEEN_RANK: usize = JACK_RANK + 1;
const KING_RANK: usize = QUEEN_RANK + 1;
const ACE_RANK: usize = KING_RANK + 1;

const PLAYER_HAND_SIZE: usize = 3;
const PLAYER_BOARD_SIZE: usize = 3;
const PLAYER_HAND_BOARD_SIZE: usize = PLAYER_HAND_SIZE + PLAYER_BOARD_SIZE * 2;

const NO_CARD: &str = "   ";
const CANT_PLAY_CARD: &str = "You can't play this card, use the `take` command to take the stack.";
const FACE_DOWN_CARD_NO_PLAYABLE: &str = "You couldn't play this card.";
const OUT_OF_BOUND_CARD_SELECTION: &str = "Select a card in bound of your hand or your board.";
const NO_CARD_ON_STACK: &str = "No card left on this stack.";
const NO_CARD_TO_TAKE: &str = "no card to take";
const INVALID_COMMAND: &str = "Enter a number or use one of these commands: exit, take, sort.";
const MULTIPLE_SELECTION_DIFFERENT_VALUE: &str = "If you play multiple cards, they need to have the same rank.";

const CARD_UNICODE_CHARS: &[char] = &[
	'🃒', '🃓', '🃔', '🃕', '🃖', '🃗', '🃘', '🃙', '🃚', '🃛', '🃜', '🃝', '🃞', '🃑', '🃂', '🃃', '🃄', '🃅', '🃆',
	'🃇', '🃈', '🃉', '🃊', '🃋', '🃌', '🃍', '🃎', '🃁', '🂲', '🂳', '🂴', '🂵', '🂶', '🂷', '🂸', '🂹', '🂺', '🂻',
	'🂼', '🂽', '🂾', '🂱', '🂢', '🂣', '🂤', '🂥', '🂦', '🂧', '🂨', '🂩', '🂪', '🂫', '🂬', '🂭', '🂮', '🂡',
];

// The color of the card.
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(PartialEq, Eq, Clone)]
enum Suit {
	// ♣
	Club,
	// ♦
	Diamond,
	// ♥
	Heart,
	// ♠
	Spade,
}
impl fmt::Display for Suit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Club => write!(f, "♣"),
			Self::Diamond => write!(f, "♦"),
			Self::Heart => write!(f, "♥"),
			Self::Spade => write!(f, "♠"),
		}
	}
}

#[cfg_attr(debug_assertions, derive(Debug))]
enum BoundError {
	OutOfBound {
		min: usize,
		max: usize,
		value: usize,
	},
}

type Numeral = usize;

struct InvalidRankValue(usize);

/// The rank of the card, in order.
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(PartialEq, Eq, Clone)]
enum Rank {
	Ace,
	King,
	Queen,
	Jack,
	Numeral(Numeral),
}
impl Rank {
	/// Returns the value of the `Rank`.
	fn value(&self) -> usize {
		match self {
			Self::Ace => ACE_RANK,
			Self::King => KING_RANK,
			Self::Queen => QUEEN_RANK,
			Self::Jack => JACK_RANK,
			Self::Numeral(value) => *value,
		}
	}
}
impl TryFrom<usize> for Rank {
	type Error = InvalidRankValue;

	fn try_from(value: usize) -> Result<Self, Self::Error> {
		match value {
			ACE_RANK => Ok(Self::Ace),
			KING_RANK => Ok(Self::King),
			QUEEN_RANK => Ok(Self::Queen),
			JACK_RANK => Ok(Self::Jack),
			MIN_NUMERAL_CARD_RANK..=MAX_NUMERAL_CARD_RANK => Ok(Self::Numeral(value)),
			_ => Err(InvalidRankValue(value)),
		}
	}
}
impl PartialOrd for Rank {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}
impl Ord for Rank {
	fn cmp(&self, other: &Self) -> Ordering {
		self.value().cmp(&other.value())
	}
}
impl fmt::Display for Rank {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Ace => write!(f, "Ace"),
			Self::King => write!(f, "King"),
			Self::Queen => write!(f, "Queen"),
			Self::Jack => write!(f, "Jack"),
			Self::Numeral(value) => write!(f, "{value}"),
		}
	}
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Eq)]
struct Card {
	color: Suit,
	value: Rank,
	icon: Option<char>,
}
impl Card {
	/// Returns a new `Card`.
	fn new(color: Suit, value: Rank) -> Self {
		Self {
			color,
			value,
			icon: None,
		}
	}
	/// Return `true` if the `Card` is of the same value.
	fn is(&self, value: usize) -> bool {
		if let Ok(rank) = Rank::try_from(value) {
			self.value == rank
		} else {
			false
		}
	}
}
impl PartialEq for Card {
	fn eq(&self, other: &Card) -> bool {
		self.value == other.value
	}
}
impl PartialOrd for Card {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}
impl Ord for Card {
	fn cmp(&self, other: &Self) -> Ordering {
		self.value.cmp(&other.value)
	}
}
impl fmt::Display for Card {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(icon_char) = self.icon {
			write!(f, "{icon_char} ")
		} else {
			write!(f, "{c}{v}{c}", c = self.color, v = self.value)
		}
	}
}

#[cfg_attr(debug_assertions, derive(Debug))]
struct Deck(Vec<Card>);
impl Deck {
	fn get_last_card_that_is_not(&self, value: usize) -> Option<&Card> {
		for i in (0..self.0.len()).rev() {
			if !self.0[i].is(value) {
				return self.0.get(i);
			}
		}

		None
	}
}
impl Deref for Deck {
	type Target = Vec<Card>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}
impl DerefMut for Deck {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}
impl fmt::Display for Deck {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut to_print = "[".to_string();
		for card in &self.0 {
			write!(to_print, "{card} | ").expect("can't display deck");
		}
		to_print.truncate(to_print.len() - 3);
		to_print.push(']');
		write!(f, "{to_print}")
	}
}

#[cfg_attr(debug_assertions, derive(Debug))]
struct FaceDownFaceUpStack {
	face_up: Option<Card>,
	face_down: Option<Card>,
}
impl FaceDownFaceUpStack {
	fn new(face_up: Card, face_down: Card) -> Self {
		Self {
			face_up: Some(face_up),
			face_down: Some(face_down),
		}
	}
	/// Returns the top stack `Card`, `None` if no `Card` left.
	fn pop(&mut self) -> Option<Card> {
		if self.face_up.is_some() {
			self.face_up.take()
		} else {
			self.face_down.take()
		}
	}
	/// Returns a ref to the face up `Card`, `None` if face down or empty.
	fn see_face_up_card(&self) -> Option<&Card> {
		self.face_up.as_ref()
	}
	/// Returns if there is a `Card` left.
	fn is_empty(&self) -> bool {
		self.face_up.is_none() && self.face_down.is_none()
	}
	/// Returns if the top `Card` is face up.
	/// Returns `None` if `Card` is face down or none left.
	fn is_visible(&self) -> Option<bool> {
		if self.face_up.is_some() {
			Some(true)
		} else if self.face_down.is_some() {
			Some(false)
		} else {
			None
		}
	}
}
impl fmt::Display for FaceDownFaceUpStack {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let face_up_string = self.see_face_up_card().map(ToString::to_string);
		let to_print: String = if let Some(s) = face_up_string {
			s
		} else if !self.is_empty() {
			"???".to_string()
		} else {
			NO_CARD.to_string()
		};
		write!(f, "{to_print}")
	}
}

type PlayerBoard = [FaceDownFaceUpStack; PLAYER_BOARD_SIZE];
type Hand = Vec<Card>;

fn display_hand(hand: &Hand) -> String {
	let mut to_print = "[".to_string();
	for card in hand {
		write!(to_print, "{card} | ").expect("can't display deck");
	}
	to_print.truncate(to_print.len() - 3);
	to_print.push(']');
	to_print
}

/// Not sure of the naming.
/// Get a ref of a face up card(hand or player board)
/// or a face down card.
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(PartialEq)]
enum CardView<'player> {
	/// View into a `Card` in hand of visible on the player board.
	View(&'player Card),
	/// The `Card` face down on the player board.
	FaceDownCard(Card),
	/// Out of bound in the hand or the player board.
	OutOfBound,
	/// No `Card` left on this stack of the player board.
	NoCard,
}

#[cfg_attr(debug_assertions, derive(Debug))]
struct Player {
	name: String,
	personal_board: PlayerBoard,
	hand: Hand,
}
impl<'player> Player {
	/// Returns if the `Player` has won, i.e. if his hand and board are empty.
	fn has_won(&self) -> bool {
		self.personal_board[0].is_empty()
			&& self.personal_board[1].is_empty()
			&& self.personal_board[2].is_empty()
			&& self.hand.is_empty()
	}
	/// Returns a ref of the `Card` to be played or the `Card` if it's face down.
	fn get_card(&'player mut self, index: usize) -> CardView<'player> {
		if !self.hand.is_empty() {
			self.hand
				.get(index)
				.map_or_else(|| CardView::OutOfBound, CardView::View)
		} else if index < PLAYER_BOARD_SIZE {
			let mut fufds = &mut self.personal_board[index];
			if let Some(visible) = fufds.is_visible() {
				if visible {
					CardView::View(fufds.see_face_up_card().unwrap())
				} else {
					CardView::FaceDownCard(fufds.pop().unwrap())
				}
			} else {
				CardView::NoCard
			}
		} else {
			CardView::OutOfBound
		}
	}
	/// Returns a ref to the `Card` at index.
	fn see_card(&'player self, index: usize) -> Option<&'player Card> {
		if !self.hand.is_empty() {
			self.hand.get(index)
		} else if index < PLAYER_BOARD_SIZE {
			let mut fufds = &self.personal_board[index];
			fufds.see_face_up_card()
		} else {
			None
		}
	}
	/// Returns the `Card` at the index, or None if no `Card`.
	fn pop_card(&mut self, index: usize) -> Option<Card> {
		if !self.hand.is_empty() {
			Some(self.hand.remove(index))
		} else if index < PLAYER_BOARD_SIZE {
			self.personal_board[index].pop()
		} else {
			None
		}
	}
}
impl fmt::Display for Player {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"[{} | {} | {}]\n{}",
			self.personal_board[0],
			self.personal_board[1],
			self.personal_board[2],
			display_hand(&self.hand),
		)
	}
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq)]
enum CardOrder {
	Ascending,
	Descending,
}
impl fmt::Display for CardOrder {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Ascending => write!(f, "ascending"),
			Self::Descending => write!(f, "descending"),
		}
	}
}

/// Returns the order of the next card.
fn resolve_card_order(last_card_played: Option<&Card>) -> CardOrder {
	if let Some(last_card_played) = last_card_played {
		if last_card_played.is(7) {
			CardOrder::Descending
		} else {
			CardOrder::Ascending
		}
	} else {
		CardOrder::Ascending
	}
}

/// Returns if the card can be played.
fn can_play_card(last_card: Option<&Card>, to_play_card: &Card, card_order: CardOrder) -> bool {
	if to_play_card.is(2) || to_play_card.is(3) {
		return true;
	}

	if let Some(last_card) = last_card {
		match card_order {
			CardOrder::Ascending => last_card <= to_play_card,
			CardOrder::Descending => last_card >= to_play_card,
		}
	} else {
		true
	}
}

#[cfg_attr(debug_assertions, derive(Debug))]
struct Game {
	players: Vec<Player>,
	drawing_stack: Deck,
	playing_stack: Deck,
}
impl Game {
	/// Start the main loop gameplay.
	fn start(&mut self) {
		let mut player_replay = false;
		for i in (0..self.players.len()).cycle() {
			if player_replay {
				player_replay = false;
				continue;
			}

			player_turn(
				&mut self.players[i],
				&mut self.drawing_stack,
				&mut self.playing_stack,
				&mut player_replay,
			);
		}
	}
}

fn player_turn(
	player: &mut Player,
	drawing_stack: &mut Deck,
	playing_stack: &mut Deck,
	player_replay: &mut bool,
) {
	let last_played_card_str = {
		let last_played_card = playing_stack.last();
		last_played_card.map_or(NO_CARD.to_string(), ToString::to_string)
	};

	let mut is_selection_valid = false;
	while !is_selection_valid {
		println!("-----");
		println!("Last played card: [{last_played_card_str}]");
		println!("{} turns...", player.name);
		println!("Your board and hand:\n{player}");

		let last_card = playing_stack.get_last_card_that_is_not(3);
		let card_order = resolve_card_order(last_card);

		let mut buffer = String::new();
		let _ = io::stdin().read_line(&mut buffer);
		let buffer = buffer.replace(' ', "");
		let buffer = buffer.trim_end(); // Remove new line char.

		let selected_indexes: Result<Vec<usize>, _> = buffer.split(',').map(str::parse::<usize>).collect();

		if let Ok(selected_indexes) = selected_indexes {
			if selected_indexes.len() > 1
				&& !check_multiple_played_card(player, &selected_indexes) {
				println!("{MULTIPLE_SELECTION_DIFFERENT_VALUE}");
				continue;
			}

			resolve_player_selected_index_card_event(
				player,
				&selected_indexes,
				drawing_stack,
				playing_stack,
				card_order,
				&mut is_selection_valid,
				player_replay,
			);
		} else {
			execute_player_command(
				buffer.trim_end(),
				playing_stack,
				player,
				&mut is_selection_valid,
			);
		}
	}
}

fn check_multiple_played_card(player: &Player, selected_indexes: &[usize]) -> bool {
	let mut cards_value: Option<Rank> = None;

	for index in selected_indexes {
		let card_view = player.see_card(*index);
		if let Some(card_view) = card_view {
			if let Some(ref value) = cards_value {
				if card_view.value != *value {
					return false;
				}
			} else {
				cards_value = Some(card_view.value.clone());
			}
		} else {
			return false;
		}
	}

	true
}

fn resolve_player_selected_index_card_event(
	player: &mut Player,
	selected_indexes: &[usize],
	drawing_stack: &mut Deck,
	playing_stack: &mut Deck,
	card_order: CardOrder,
	is_selection_valid: &mut bool,
	player_replay: &mut bool,
) {
	let first_index = selected_indexes[0];
	let card_view = player.get_card(first_index);
	match card_view {
		CardView::View(card_ref) => {
			let last_card = playing_stack.get_last_card_that_is_not(3);
			if can_play_card(last_card, card_ref, card_order) {
				let card = player.pop_card(first_index).unwrap();
				if card.is(8) {
					*player_replay = true;
				}
				if card.is(10) || selected_indexes.len() == 4 {
					playing_stack.clear();
				} else {
					// The first card is already poped.
					let mut cards_to_push = Vec::with_capacity(selected_indexes.len() - 1);
					for i in 1..selected_indexes.len() {
						let card = player.pop_card(first_index).unwrap();
						cards_to_push.push(card);
					}
					playing_stack.append(&mut cards_to_push);
				}
				if let Some(drawn_card) = drawing_stack.pop() {
					player.hand.push(drawn_card);
				}
				*is_selection_valid = true;
			} else {
				println!("{CANT_PLAY_CARD}");
			}
		}
		CardView::FaceDownCard(face_down_card) => {
			let can_play = {
				let last_card = playing_stack.get_last_card_that_is_not(3);
				can_play_card(last_card, &face_down_card, card_order)
			};
			if can_play {
				playing_stack.push(face_down_card);
				*is_selection_valid = true;
			} else {
				println!("{FACE_DOWN_CARD_NO_PLAYABLE}");
				player.hand.push(face_down_card);
				player.hand.append(playing_stack);
			}
		}
		CardView::OutOfBound => {
			println!("{OUT_OF_BOUND_CARD_SELECTION}");
		}
		CardView::NoCard => {
			println!("{NO_CARD_ON_STACK}");
		}
	}
}

fn execute_player_command(
	command: &str,
	playing_stack: &mut Deck,
	player: &mut Player,
	is_selection_valid: &mut bool,
) {
	match command {
		"take" => {
			if playing_stack.is_empty() {
				println!("{NO_CARD_TO_TAKE}");
			} else {
				player.hand.append(playing_stack);
				*is_selection_valid = true;
			}
		}
		"sort" => player.hand.sort(),
		"exit" => std::process::exit(0),
		_ => println!("{INVALID_COMMAND}"),
	}
}

/// Returns a 52 card `Deck`.
fn create_standard_52_card_deck() -> Deck {
	let mut standard_52_card: Vec<Card> = Vec::new();

	let mut unicode_chars_index = 0;
	for color in &[Suit::Club, Suit::Diamond, Suit::Heart, Suit::Spade] {
		for i in MIN_NUMERAL_CARD_RANK..=ACE_RANK {
			let value = match i {
				ACE_RANK => Rank::Ace,
				KING_RANK => Rank::King,
				QUEEN_RANK => Rank::Queen,
				JACK_RANK => Rank::Jack,
				_ => Rank::Numeral(i),
			};

			let card = Card {
				color: color.clone(),
				value,
				icon: None, // Some(CARD_UNICODE_CHARS[unicode_chars_index]),
			};

			standard_52_card.push(card);
			unicode_chars_index += 1;
		}
	}

	Deck(standard_52_card)
}

/// Create a `Player` with a personal board and a hand from the `Deck`;
fn create_player(name: String, deck: &mut Deck) -> Player {
	let mut cards: Vec<Card> = deck.drain(..PLAYER_HAND_BOARD_SIZE).collect();
	assert_eq!(cards.len(), 9, "not enough card in the deck");

	let player_board = [
		FaceDownFaceUpStack::new(cards.pop().unwrap(), cards.pop().unwrap()),
		FaceDownFaceUpStack::new(cards.pop().unwrap(), cards.pop().unwrap()),
		FaceDownFaceUpStack::new(cards.pop().unwrap(), cards.pop().unwrap()),
	];

	Player {
		name,
		hand: cards.into_iter().collect(),
		personal_board: player_board,
	}
}

/// Shuffle a `Deck`.
fn shuffle(deck: &mut Deck, seed: Option<u64>) {
	let mut rng = if let Some(seed) = seed {
		rand_chacha::ChaCha8Rng::seed_from_u64(seed)
	} else {
		ChaCha8Rng::from_os_rng()
	};
	for i in 0..deck.len() {
		// The deck size won't exceed `u32::MAX`.
		#[allow(clippy::cast_possible_truncation)]
		let new_i = rng.next_u32() % (deck.len() as u32);
		deck.swap(i, new_i as usize);
	}
}

/// Returns a newly generated `Game`.
fn create_game(player_names: Vec<String>, seed: Option<u64>) -> Game {
	let mut drawing_stack = create_standard_52_card_deck();
	shuffle(&mut drawing_stack, seed);

	let mut players = Vec::<Player>::new();
	for name in player_names {
		let player = create_player(name, &mut drawing_stack);
		players.push(player);
	}

	Game {
		players,
		drawing_stack,
		playing_stack: Deck(Vec::new()),
	}
}

fn main() {
	// TODO: add index next to the cards.
	let mut game = create_game(vec!["GEM".to_string(), "MAR".to_string()], None);
	game.start();
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_rank_ordering() {
		assert!(Rank::King > Rank::Queen);
		assert!(Rank::Queen > Rank::Jack);
		let cinq = Rank::Numeral(5);
		let seven = Rank::Numeral(7);
		assert!(cinq < seven);
	}

	#[test]
	fn test_create_standard_52_card_deck() {
		let deck = create_standard_52_card_deck();
		assert_eq!(deck.len(), 52);
	}

	#[test]
	fn test_card_eq() {
		let card_1 = Card {
			color: Suit::Diamond,
			value: Rank::King,
		};
		let card_2 = Card {
			color: Suit::Spade,
			value: Rank::King,
		};
		assert_eq!(card_1, card_2);
	}

	#[test]
	fn test_face_up_face_down_stack() {
		let up = Card::new(Suit::Diamond, Rank::Numeral(3));
		let down = Card::new(Suit::Spade, Rank::Ace);
		let mut stack = FaceDownFaceUpStack::new(up.clone(), down.clone());
		assert_eq!(stack.is_visible(), Some(true));
		assert_eq!(stack.is_empty(), false);
		assert_eq!(stack.see_face_up_card(), Some(&up));
		assert_eq!(stack.pop(), Some(up));

		assert_eq!(stack.is_visible(), Some(false));
		assert_eq!(stack.is_empty(), false);
		assert_eq!(stack.see_face_up_card(), None);
		assert_eq!(stack.pop(), Some(down));

		assert_eq!(stack.is_visible(), None);
		assert_eq!(stack.is_empty(), true);
		assert_eq!(stack.see_face_up_card(), None);
		assert_eq!(stack.pop(), None);
	}

	#[test]
	fn test_player() {
		let up_0 = Card::new(Suit::Diamond, Rank::Numeral(3));
		let down_0 = Card::new(Suit::Spade, Rank::Ace);
		let mut stack_0 = FaceDownFaceUpStack::new(up_0.clone(), down_0);

		let up_1 = Card::new(Suit::Heart, Rank::Numeral(7));
		let down_1 = Card::new(Suit::Diamond, Rank::Jack);
		let mut stack_1 = FaceDownFaceUpStack::new(up_1, down_1);

		let up_2 = Card::new(Suit::Club, Rank::Queen);
		let down_2 = Card::new(Suit::Club, Rank::Numeral(10));
		let mut stack_2 = FaceDownFaceUpStack::new(up_2, down_2);

		let personal_board = [stack_0, stack_1, stack_2];

		let card_0 = Card::new(Suit::Heart, Rank::Numeral(2));
		let card_1 = Card::new(Suit::Spade, Rank::Numeral(6));
		let hand = vec![card_0.clone(), card_1.clone()];

		let mut player = Player {
			name: "test".to_string(),
			personal_board,
			hand,
		};

		assert!(!player.has_won());
		assert_eq!(player.get_card(2), CardView::OutOfBound);

		assert_eq!(player.get_card(0), CardView::View(&card_0));
		assert_eq!(player.pop_card(0), Some(card_0));

		assert_eq!(player.get_card(0), CardView::View(&card_1));
		assert_eq!(player.pop_card(0), Some(card_1));

		assert_eq!(player.get_card(0), CardView::View(&up_0));
		assert_eq!(player.pop_card(0), Some(up_0));
		player.pop_card(0);

		player.pop_card(1);
		player.pop_card(1);

		player.pop_card(2);
		player.pop_card(2);

		assert!(player.has_won());
	}

	#[test]
	fn test_resolve_card_order() {
		let king = Card {
			color: Suit::Spade,
			value: Rank::King,
		};
		let seven = Card {
			color: Suit::Spade,
			value: Rank::Numeral(7),
		};
		assert_eq!(resolve_card_order(None), CardOrder::Ascending);
		assert_eq!(resolve_card_order(Some(&king)), CardOrder::Ascending);
		assert_eq!(resolve_card_order(Some(&seven)), CardOrder::Descending);
	}

	#[test]
	fn test_can_play_card() {
		assert!(can_play_card(
			None,
			&Card::new(Suit::Diamond, Rank::King),
			CardOrder::Ascending,
		));
		assert!(can_play_card(
			Some(&Card::new(Suit::Spade, Rank::King)),
			&Card::new(Suit::Diamond, Rank::King),
			CardOrder::Ascending,
		));
		assert!(!can_play_card(
			Some(&Card::new(Suit::Heart, Rank::Numeral(7))),
			&Card::new(Suit::Diamond, Rank::King),
			CardOrder::Descending,
		));
		assert!(!can_play_card(
			Some(&Card::new(Suit::Spade, Rank::King)),
			&Card::new(Suit::Diamond, Rank::Jack),
			CardOrder::Ascending,
		));
		assert!(!can_play_card(
			Some(&Card::new(Suit::Heart, Rank::Numeral(7))),
			&Card::new(Suit::Diamond, Rank::Jack),
			CardOrder::Descending,
		));
	}

	#[test]
	fn test_shuffle() {
		let mut deck = create_standard_52_card_deck();
		assert_eq!(deck[0], Card::new(Suit::Club, Rank::Numeral(2)));
		assert_eq!(deck[42], Card::new(Suit::Spade, Rank::Numeral(5)));
		assert_eq!(deck[51], Card::new(Suit::Spade, Rank::Ace));
		shuffle(&mut deck, Some(42));
		assert_eq!(deck[0], Card::new(Suit::Club, Rank::Ace));
		assert_eq!(deck[42], Card::new(Suit::Heart, Rank::Ace));
		assert_eq!(deck[51], Card::new(Suit::Spade, Rank::Jack));
	}

	#[test]
	fn test_get_last_card_that_is_not() {
		let three = Card::new(Suit::Heart, Rank::Numeral(3));
		let king = Card::new(Suit::Diamond, Rank::King);
		let deck = Deck(vec![king.clone(), three.clone(), three.clone()]);
		assert_eq!(deck.get_last_card_that_is_not(3), Some(&king));
		let deck = Deck(vec![three.clone(), three.clone()]);
		assert_eq!(deck.get_last_card_that_is_not(3), None);
	}
}
