use dynparser::{parse, peg, rules_from_peg};
use std::result;

static PEG2PARSE_PEG: &str = r#"
    /*      A peg grammar to parse peg grammars
     *
     */

    main            =   grammar

    grammar         =   rule+

    rule            =   _  symbol  _  '='  _  expr  _eol _

    expr            =   or

    or              =   and         ( _  '/'  _  (error  /  or)  )?
    error           =   'error' _  '('  _  literal  _  ')'

    and             =   rep_or_neg  ( _1 _ !(symbol _ '=') and )*     rulestransf?
    _1              =   (' ' / eol)     //  this is the and separator

    rep_or_neg      =   atom_or_par ('*' / '+' / '?')?
                    /   '!' atom_or_par

    atom_or_par     =   (atom / parenth)

    parenth         =   '('  _  expr  _  ')'

    atom            =   literal
                    /   match
                    /   dot
                    /   symbol

    literal         =  lit_noesc  /  lit_esc

    lit_noesc       =   _'   (  !_' .  )*   _'
    _'              =   "'"

    lit_esc         =   _"
                            (   esc_char
                            /   hex_char
                            /   !_" .
                            )*
                        _"
    _"              =   '"'

    esc_char        =   '\r'
                    /   '\n'
                    /   '\t'
                    /   '\\'
                    /   '\"'

    hex_char        =   '\0x' [0-9A-F] [0-9A-F]

    symbol          =   [_a-zA-Z0-9] [_'"a-zA-Z0-9]*

    eol             =   ("\r\n"  /  "\n"  /  "\r")
    _eol            =   (' ' / comment)*  eol

    match           =   '['
                            (
                                (mchars  mbetween*)
                                / mbetween+
                            )
                        ']'

    mchars          =   (!']' !(. '-') .)+
    mbetween        =   (.  '-'  .)

    dot             =   '.'

    _               =   (  ' '
                        /   eol
                        /   comment
                        )*

    comment         =   line_comment
                    /   mline_comment

    line_comment    =   '//' (!eol .)*  eol
    mline_comment   =   '/*' (!'*/' .)* '*/'

    rulestransf     =   _ '->' _
                                (   line_rt
                                /   block_rt
                                )
    
    line_rt         =   (!eol .)*
    block_rt        =   _ '{'   (!'}' .)*   _ '}' _
    "#;

pub fn compile(peg_gramm: &str) -> result::Result<(), peg::Error> {
    let rules2parse_peg = rules_from_peg(PEG2PARSE_PEG)?;

    let _peg_ast = parse(peg_gramm, &rules2parse_peg)?;

    Ok(())
}
