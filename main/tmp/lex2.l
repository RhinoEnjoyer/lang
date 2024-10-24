%top{
  //I might go and auto generate somethings this
  #include "../codes/codes.hpp"
  #include "../ansicodes.hpp"
  #include <iostream>
  #include <vector>
  #include "../node.hpp"
  #include <string>
}
%option ctorarg="std::vector<snode>* n"
%option ctorinit="nodes(n)"

%class{
  public:
//  std::vector<std::string> toks;
  std::vector<loct> locs;
  std::vector<snode>*  nodes;

  size_t boffset = 0;

  private:
  tokc proc(const int type){
    const uint64_t row = lineno();
    const uint64_t col = columno() + 1;

    const uint64_t end_row = lineno_end();
    const uint64_t end_col = columno_end() + 1;

    const uint64_t index = locs.size();

    const auto obuffer = boffset;
    boffset += size();

    //toks.push_back(str());
    locs.emplace_back(row,col, end_row,end_col/*, obuffer,boffset*/);
    nodes->emplace_back(node{static_cast<tokc>(type),index});


    return static_cast<tokc>(type);
  }
}


ID [[:alpha:]][[:word:]]*
_INT [[:digit:]]+
SU ["S""U"]
EXPNOT "e"[(\+)(\-)]{_INT}

TONE_INT (-?)({_INT}')+({EXPNOT}?){SU}?
INT (-?){_INT}({EXPNOT}?){SU}?
FLOAT (-?){_INT}"."{_INT}{EXPNOT}?

STRLIT L?\"(\\.|\\\r?\n|[^\\\n"])*\"
CHARLIT   L?'(\\.|[^\\\n'])*'

%%

"//".*\n
[[:space:]]+ 

0[xX][[:xdigit:]]+  {proc(HEX);}
0[bB][01]+          {proc(BIN);}
0[oO][0-7]+          

{STRLIT} {proc(STRLIT);}
{CHARLIT} {proc(CHARLIT);}

"@tag"       {proc(BUILTIN_TAG);}
"@type"      {proc(BUILTIN_TYPE);}
"@vec"       {proc(BUILTIN_VECTOR);}
"@array"     {proc(BUILTIN_ARRAY);}
"@tup"       {proc(BUILTIN_TUPLE);}
"@it"        {proc(BUILTIN_ITERATOR);}
"@fn"        {proc(BUILTIN_FN);}
"@rec"       {proc(BUILTIN_REC);}
"@union"     {proc(BUILTIN_UNION);}
"@enum"      {proc(BUILTIN_ENUM);}
"@range"     {proc(BUILTIN_RANGE);}
"@alias"     {proc(BUILTIN_ALIAS);}
"@scope"     {proc(BUILTIN_SCOPE);}
"@number"    {proc(BUILTIN_NUMBER);}

"@hex"       {proc(BUILTIN_HEX);}
"@oct"       {proc(BUILTIN_OCT);}
"@bin"       {proc(BUILTIN_BIN);}

"@int"       {proc(BUILTIN_INT);}
"@sint"      {proc(BUILTIN_SINT);}
"@uint"      {proc(BUILTIN_UINT);}
"@float"     {proc(BUILTIN_FLOAT);}

"@mut"       {proc(BUILTIN_MUTABLE);}
"@imut"      {proc(BUILTIN_IMMUTABLE);}

"@extern"    {proc(BUILTIN_EXTERN);}

"@pub"       {proc(BUILTIN_PUBLIC);}
"@priv"      {proc(BUILTIN_PRIVATE);}

"@ptr"       {proc(BUILTIN_POINTER);}
"@ref"       {proc(BUILTIN_REFRENCE);}

"@allocfn"   {proc(BUILTIN_ALLOCFN);}
"@reallocfn" {proc(BUILTIN_REALLOCFN);}
"@freefn"    {proc(BUILTIN_FREEFN);}

"@defer"     {proc(BUILTIN_DEFER);}

"@any"       {proc(BUILTIN_ANY);}
"@sizeof"    {proc(BUILTIN_SIZEOF);}
"@typeof"    {proc(BUILTIN_TYPEOF);}
"@countof"   {proc(BUILTIN_COUNTOF);}
"@as"        {proc(BUILTIN_AS);}
"@ret"       {proc(BUILTIN_RETURN);}
"@import"    {proc(BUILTIN_IMPORT);}
"@pass"      {proc(BUILTIN_PASS);}
"@lengthof"  {proc(BUILTIN_LENGTHOF);}

"@while"     {proc(BUILTIN_WHILE);}
"@for"       {proc(BUILTIN_FOR);}

"@pipe"      {proc(BUILTIN_PIPE);}

"@if"        {proc(BUILTIN_IF);}
"@elif"      {proc(BUILTIN_ELIF);}
"@else"      {proc(BUILTIN_ELSE);}

"@switch"    {proc(BUILTIN_SWITCH);}
"@case"      {proc(BUILTIN_CASE);}
"@default"   {proc(BUILTIN_CASE);}

"@set"       {proc(BUILTIN_SET);}
"@auto"      {proc(BUILTIN_AUTO);}

"@suffix"    {proc(BUILTIN_SUFFIX);}
"@prefix"    {proc(BUILTIN_PREFIX);}

"@lmin"      {proc(BUILTIN_LIMIT_MIN);}
"@lmax"      {proc(BUILTIN_LIMIT_MAX);}

"@0"         {proc(BUILTIN_NULL);}
"@null"      {proc(BUILTIN_NULL);}

"@inimitable" {proc(BUILTIN_LIMIT_MAX);}
"@imitable"   {proc(BUILTIN_LIMIT_MAX);}

"@opadd"     {proc(BUILTIN_LIMIT_MAX);}
"@opsub"     {proc(BUILTIN_LIMIT_MAX);}
"@opdiv"     {proc(BUILTIN_LIMIT_MAX);}
"@opmul"     {proc(BUILTIN_LIMIT_MAX);}
"@opmod"     {proc(BUILTIN_LIMIT_MAX);}
"@opand"     {proc(BUILTIN_LIMIT_MAX);}
"@opor"      {proc(BUILTIN_LIMIT_MAX);}
"@opxor"     {proc(BUILTIN_LIMIT_MAX);}
"@opeq"      {proc(BUILTIN_LIMIT_MAX);}
"@opneq"     {proc(BUILTIN_LIMIT_MAX);}
"@opgeq"     {proc(BUILTIN_LIMIT_MAX);}
"@opleq"     {proc(BUILTIN_LIMIT_MAX);}
"@opless"    {proc(BUILTIN_LIMIT_MAX);}
"@opgreater" {proc(BUILTIN_LIMIT_MAX);}

"@"{ID} {proc(BUILTIN_ID);}

"@" {proc(DUCKLING);}


"{" { return proc(LCBRACE);}
"}" { return proc(RCBRACE);}

"[" { return proc(LBRACE);}
"]" { return proc(RBRACE);}

"(" { return proc(LPAREN);}
")" { return proc(RPAREN);}

":<" { return proc(LANGLE);}
">:" { return proc(RANGLE);}

{FLOAT} {proc(FLOAT);}
{TONE_INT} {proc(TONE_INT);}
{INT} {proc(INT);}
{ID} {proc(ID);}

"#" {proc(HASH);}

"&&"  {proc(OP_AND);}
"||" {proc(OP_OR);}
"^|" {proc(OP_XOR);}

"==" {proc(OP_EQUAL);}
"!=" {proc(OP_NEQUAL);}
">=" {proc(OP_GEQUAL);}
"<=" {proc(OP_LEQUAL);}
"+=" {proc(OP_PLUSASIGN);}
"-=" {proc(OP_MINUSASIGN);}
"*=" {proc(OP_MULASIGN);}
"/=" {proc(OP_DIVASIGN);}

"^"    {proc(CARET);}
"&"    {proc(AMPERSAND);}

"..."  {proc(ELIPSIS);}
"."    {proc(DOT);}

"::"   {proc(DCOLON);}
":"    {proc(COLON);}
";"    {proc(SCOLON);}
"'"    {proc(TONE);}
","    {proc(COMMA);}

"->"    {proc(OP_RPIPE);}

"="   {proc(ASIGN);}

"<<"  {proc(OP_LSHIFT);}
">>"  {proc(OP_RSHIFT);}
">"  {proc(OP_GREATER);}
"<"  {proc(OP_LESS);}

"|"  {proc(LINE);}
"~"  {proc(PERISPOMENI);}
"!"  {proc(OP_NOT);}
"?"  {proc(QMARK);}

"++" {proc(OP_PLUSPLUS);}
"--" {proc(OP_MINUSMINUS);}
"+" {proc(OP_PLUS);}
"-" {proc(OP_MINUS);}
"*" {proc(OP_MUL);}
"/" {proc(OP_DIV);}
"%" {proc(OP_MOD);}

. {proc(OTHER);}

<<EOF>> {return ENDTKN;}
%%

