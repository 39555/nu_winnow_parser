#![allow(unused_imports)]
#![allow(unused_variables)]
use winnow::{
    ascii::{digit1, line_ending, multispace0, multispace1},
    combinator::{
        alt, cut_err, delimited, dispatch, eof, fail, iterator, opt, peek, preceded, repeat,
        separated, separated_pair, terminated, trace,
    },
    error::ParserError,
    prelude::*,
    token::{any, literal, one_of, take, take_till, take_until, take_while},
    PResult, Parser,
};
use std::fmt;
use winnow::error::ErrMode;
use winnow::error::{ContextError, ErrorKind};
use winnow::stream::Stream;

use nu_cmd_lang;
use nu_path;
use nu_protocol::engine::{EngineState, StateWorkingSet};
//use nu_protocol::{
//    ast, engine::StateWorkingSet, eval_const::eval_constant, span, BlockId, DidYouMean, Flag,
//    ParseError, PositionalArg, Signature, Span, Spanned, SyntaxShape, Type, VarId, ENV_VARIABLE_ID,
//    IN_VARIABLE_ID,
//};
use std::{
    collections::{HashMap, HashSet},
    num::ParseIntError,
    str,
    sync::Arc,
};

#[derive(Debug)]
pub enum Tok {
    Number,
    Comma,
    String,
    Dollar,
    Dot,
    DotDot,
    Name,
    Pipe,
    PipePipe,
    Colon,
    ColonColon,
    Semicolon,
    Plus,
    PlusPlus,
    PlusEquals,
    Dash,
    DashEquals,
    Exclamation,
    Asterisk,
    AsteriskAsterisk,
    AsteriskEquals,
    ForwardSlash,
    ForwardSlashForwardSlash,
    ForwardSlashEquals,
    Equals,
    EqualsEquals,
    EqualsTilde,
    ExclamationTilde,
    ExclamationEquals,
    LParen,
    LSquare,
    LCurly,
    LessThan,
    LessThanEqual,
    RParen,
    RSquare,
    RCurly,
    GreaterThan,
    GreaterThanEqual,
    Ampersand,
    AmpersandAmpersand,
    QuestionMark,
    ThinArrow,
    ThickArrow,
    Newline,
    ErrGreaterThanPipe,
    OutErrGreaterThanPipe,
    OutGreaterThan,
    OutGreaterGreaterThan,
    ErrGreaterThan,
    ErrGreaterGreaterThan,
    OutErrGreaterThan,
    OutErrGreaterGreaterThan,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Int,
    Float,
    String,
    Name,
    Type,
    Variable,

    // Booleans
    True,
    False,

    // Empty values
    Null,

    // Operators
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Plus,
    Append,
    Minus,
    Multiply,
    Divide,
    // Modulo,
    And,
    Or,
    Pow,

    // Assignments
    Assignment,
    AddAssignment,
    SubtractAssignment,
    MultiplyAssignment,
    DivideAssignment,

    // Statements
    Let,
    While,
    For,
    Loop,
    Return,
    Break,
    Continue,

    // Definitions
    Def,
    Params,
    Param,
    Closure,

    // Expressions
    Call,
    NamedValue,
    BinaryOp,
    Range,
    List,
    Table,
    Record,
    MemberAccess,
    Block,
    If,
    Match,
    Statement,
    Garbage,
}

fn trim<'i, F, O, E>(inner: F) -> impl Parser<Input<'i>, O, E>
where
    E: ParserError<Input<'i>>,
    F: Parser<Input<'i>, O, E>,
{
    delimited(multispace0, inner, multispace0)
}
//pub(crate) const WSCHAR: (u8, u8) = (b' ', b'\t');
//pub(crate) fn ws<'i>(input: &mut Input<'i>) -> PResult<&'i str> {
//    take_while(0.., WSCHAR).map(|b| "hello").parse_next(input)
//}

mod record {
    use super::*;
    fn parse_key<'i>(i: &mut Input<'i>) -> PResult<&'i str> {
        trace(
            "record_key",
            preceded(multispace0, take_till(1.., (':', ' '))),
        )
        .try_map(std::str::from_utf8)
        .parse_next(i)
    }

    struct Value {}
    // TODO:  recursion
    fn parse_value<'i>(i: &mut Input<'i>) -> PResult<Value> {
        trace(
            "record_value",
            terminated(
                preceded(multispace0, take_till(1.., (' ', ','))),
                take_while(0.., (',', ' ')),
            ),
        )
        .map(|_| Value {})
        .parse_next(i)
    }

    pub fn parse_record<'s, 'i>(
        state: &'s RefCell<impl ParseState + 's>,
    ) -> impl Parser<Input<'i>, (), ContextError> + 's {
        move |i: &mut Input<'i>| {
            trace(
                "record",
                trim(delimited(
                    '{',
                    repeat(
                        1..,
                        separated_pair(parse_key.with_span(), trim(':'), parse_value.with_span()),
                    ),
                    '}',
                )),
            )
            .with_span()
            .map(
                |(kv_with_span, span): (Vec<((&'i str, _), (Value, _))>, _)| {
                    state.borrow_mut();
                },
            )
            .parse_next(i)
        }
    }
}

mod function {
    use super::*;
    fn parse_parameters<'a>(i: &mut &'a str) -> PResult<Vec<&'a str>> {
        todo!()
    }
    fn parse_body<'a>(i: &mut &'a str) -> PResult<Vec<&'a str>> {
        todo!()
    }
    pub fn parse_cmd_identifier<'s, 'i>(
        state: &'s RefCell<impl ParseState>,
    ) -> impl Parser<Input<'i>, (), ContextError> + 's {
        move |i: &mut Input<'i>| {
            todo!()
            //trace(
            //    "commands",
            //    trim(delimited(
            //        "def",
            //        separated(1.., trim(take_till(1.., '[')), ' '),
            //        '[',
            //    )),
            //)
            //.parse_next(i)
        }
    }
    fn parse_decl_def<'s, 'i>(
        state: &'s RefCell<impl ParseState>,
    ) -> impl Parser<Input<'i>, (), ContextError> + 's {
        move |i: &mut Input<'i>| {
            todo!()
            //trace(
            //    "function",
            //    trim(parse_def_name(state)).span().map(|span| {
            //        state.borrow_mut();
            //    }),
            //)
            //.parse_next(i)
        }
    }
}
use std::ops::RangeInclusive;
// non-ascii = %x80-D7FF / %xE000-10FFFF
// - ASCII is 0xxxxxxx
// - First byte for UTF-8 is 11xxxxxx
// - Subsequent UTF-8 bytes are 10xxxxxx
pub(crate) const NON_ASCII: RangeInclusive<u8> = 0x80..=0xff;

// non-eol = %x09 / %x20-7E / non-ascii
pub(crate) const NON_EOL: (u8, RangeInclusive<u8>, RangeInclusive<u8>) =
    (0x09, 0x20..=0x7E, NON_ASCII);

mod pipeline {
    use super::*;

    // TODO: pipeline data
    pub struct Pipeline {}

    pub fn parse_pipeline<'a>(i: &mut Input<'a>) -> PResult<Pipeline> {
        trace(
            "pipeline",
            //With 4:       2 | 3  \n  | 4
            //Without 4:    2 | 3  \n    4
            terminated(
                (command::parse_pipe_element.with_span(), opt(comment)),
                opt(alt((
                    literal('|'),
                    trace(
                        "pipe_next_line",
                        preceded(
                            repeat(0.., trace("comment_on_next_line", (line_ending, comment)))
                                .map(|()| ()),
                            (line_ending, opt(comment), preceded(multispace0, '|')).recognize(),
                        ),
                    ),
                ))),
            ),
        )
        .map(|((pipe_body, span), _)| Pipeline {})
        .parse_next(i)
    }
}

mod command {
    use super::*;
    pub struct Command {}
    pub(crate) fn parse_pipe_element<'i>(i: &mut Input<'i>) -> PResult<Command> {
        trace("command", take_till(1.., ('|', '#', '\r', '\n')))
            .with_span()
            .map(|(cmd, span)| {
                println!("{}", std::str::from_utf8(cmd).unwrap());
                Command {}
            })
            .parse_next(i)
    }
}

//pub(crate) fn line_ending_or_eof<'i>(i: &mut Input<'i>) -> PResult<&'i str> {
//    alt((line_ending.value("\n"), eof.value(""))).parse_next(i)
//}

mod variable {
    use super::*;

    fn parse_var_name<'a>(i: &mut Input<'a>) -> PResult<&'a str> {
        trace("var_name", take_till(1.., (' ', '=')))
            .try_map(std::str::from_utf8)
            .parse_next(i)
    }
    fn parse_value<'a>(i: &mut Input<'a>) -> PResult<Vec<pipeline::Pipeline>> {
        trace(
            "var_value",
            // TODO how to handle as separated(1.., ) ? without checking eof
            terminated(repeat(1.., pipeline::parse_pipeline), opt(line_ending)),
        )
        .parse_next(i)
    }

    fn var<'a>(i: &mut Input<'a>) -> PResult<(Span, Vec<pipeline::Pipeline>)> {
        separated_pair(parse_var_name.span(), trim('='), parse_value).parse_next(i)
    }

    //
    pub fn parse_var_decl<'s, 'i>(
        state: &'s RefCell<impl ParseState + 's>,
    ) -> impl Parser<Input<'i>, (), ContextError> + 's {
        move |i: &mut Input<'i>| {
            dispatch! {
                peek(opt::<_, &[u8], _, _>(take(3usize)));
                    Some(b"let") => {
                        (trace("stmt_let", 
                               preceded(take(3usize), trim(var))),
                         opt(parse_comment(state))
                         )
                        .map(|((name_span, value), _)| {state.borrow_mut().on_stmt_let(name_span, value);})
                    },
                    Some(b"mut") => {
                        (trace("stmt_mut", 
                               preceded(take(3usize), trim(var))), 
                         opt(parse_comment(state)))
                        .map(|((name_span, value), _)| {state.borrow_mut().on_stmt_mut(name_span, value);})
                    },
                    _ => fail
            }
            .parse_next(i)
        }
    }
}
pub(crate) fn comment<'i>(input: &mut Input<'i>) -> PResult<&'i [u8]> {
    trace("comment", ('#', take_while(0.., NON_EOL)).recognize()).parse_next(input)
}
pub(crate) fn parse_comment<'s, 'i>(
    state: &'s RefCell<impl ParseState + 's>,
) -> impl Parser<Input<'i>, (), ContextError> + 's {
    move |i: &mut Input<'i>| {
        (comment, line_ending)
            .span()
            .map(|span| {
                state.borrow_mut().on_comment(span);
            })
            .parse_next(i)
    }
}

// TODO var decl is a stmt also
mod declaration {
    use super::*;
    use function::*;
    use variable::*;

    pub fn parse_decl<'s, 'i>(
        state: &'s RefCell<impl ParseState + 's>,
    ) -> impl Parser<Input<'i>, (), ContextError> + 's {
        move |i: &mut Input<'i>| {
            alt((
                parse_var_decl(state), // parse_def
            ))
            .parse_next(i)
        }
    }
}

mod statement {
    use super::*;
    pub fn parse_stmt<'s, 'i>(
        state: &'s RefCell<impl ParseState + 's>,
    ) -> impl Parser<Input<'i>, (), ContextError> + 's {
        move |i: &mut Input<'i>| todo!()
    }
}

mod block {
    use super::*;
    use declaration::*;
    use statement::*;

    pub fn parse_block<'s, 'i>(
        state: &'s RefCell<impl ParseState + 's>,
    ) -> impl Parser<Input<'i>, (), ContextError> + 's {
        move |i: &mut Input<'i>| {
            trace(
                "block",
                repeat(
                    0..,
                    trim(dispatch! {
                        peek::<_, u8,_,_>(any);
                            b'#' => parse_comment(state),
                            _ => alt((parse_decl(state), )), // parse_stmt(state)
                    }),
                ),
            )
            .parse_next(i)
        }
    }
}
type Input<'b> = winnow::Located<&'b winnow::BStr>;
use std::cell::RefCell;

//struct ParseState {}

pub fn parse_program<'s, 'i>(
    state: &'s RefCell<impl ParseState + 's>,
) -> impl Parser<Input<'i>, (), ContextError> + 's {
    move |i: &mut Input<'i>| {
        trace("program", (block::parse_block(state), eof))
            .void()
            .parse_next(i)
    }
}
type Span = std::ops::Range<usize>;
pub trait ParseState {
    fn on_comment(&mut self, span: Span) {}
    fn on_stmt_let(&mut self, var_name: Span, value: Vec<pipeline::Pipeline>) {}
    fn on_stmt_mut(&mut self, var_name: Span, value: Vec<pipeline::Pipeline>) {}
}

//impl From<Span> for nu_protocol::Span {
fn to_nu_span(v: Span) -> nu_protocol::Span {
    nu_protocol::Span::new(v.start, v.end)
}
//}

impl ParseState for StateWorkingSet<'_> {
    fn on_stmt_let(&mut self, var_name: Span, value: Vec<pipeline::Pipeline>) {
        let span = to_nu_span(var_name);
        let var_name = self.get_span_contents(span);
        println!(
            "stmt_let var_name: {}-{} {}",
            span.start,
            span.end,
            std::str::from_utf8(var_name).unwrap()
        );
        // TODO: String for now
        let id = self.add_variable(var_name.to_vec(), span, nu_protocol::Type::String, false);
        // TODO value expression
        //(
        //    nu_protocol::ast::Expression {
        //        expr: nu_protocol::ast::Expr::VarDecl(id),
        //        span: span,
        //        ty: nu_protocol::Type::String,
        //        custom_completion: None,
        //    },
        //    Some(nu_protocol::Type::String),
        //)
    }
}

fn main() {
    let input = "
# comment 1
let var = 5 | 3 
# comment 1
| 4
# comment 3
# comment 4
| 12
let var = 10
# sdfdf
let var2 = 11";
    let mut engine_state = nu_cmd_lang::create_default_context();
    let working_set = RefCell::new(StateWorkingSet::new(&mut engine_state));
    {
        let state_ref = &working_set;
        let _ = working_set.borrow_mut().add_file("test".into(), input.as_bytes());
        let res = parse_program(state_ref)
            .parse(winnow::Located::new(winnow::BStr::new(input)))
            .unwrap();
    }
}
//
//#[cfg(test)]
//mod tests {
//    use super::*;
//    use std::str;
//
//    #[test]
//    fn parse_simple_record() {
//        let mut input = r#"{ a: 1 }"#.as_bytes();
//        let res = record::parse_record.parse_next(&mut input).unwrap();
//    }
//}
