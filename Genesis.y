// bison easy.y --report=all -o easy.c.re
// re2c easy.c.re -o easy.c
// g++ easy.c -std=c++17 -o easy

%skeleton "lalr1.cc"

%define api.parser.class { easy_parser }
%define api.token.constructor
%define api.value.type variant

%define parse.assert
%define parse.error verbose

%locations

%code requires
{
#include <cstdio>
#include <cstdlib>

#include <cstdint>
#include <climits>

#include <list>
#include <map>
#include <vector>

#include <string>

enum IdentInfo
{
  IDENT_INFO_NONE,

  IDENT_INFO_VAR,
  IDENT_INFO_FUNC,
  IDENT_INFO_ARG,
};

enum TypeInfo
{
  TYPE_INFO_NONE,

  TYPE_INFO_U8,
  TYPE_INFO_U16,
  TYPE_INFO_U32,
  TYPE_INFO_U64,

  TYPE_INFO_I8,
  TYPE_INFO_I16,
  TYPE_INFO_I32,
  TYPE_INFO_I64,

  TYPE_INFO_R32,
  TYPE_INFO_R64,
};

enum ExprInfo
{
  EXPR_INFO_NONE,

  EXPR_INFO_COPY,
  EXPR_INFO_COMMA,

  EXPR_INFO_IDENT,
  EXPR_INFO_SIGNED_INTEGER,
  EXPR_INFO_UNSIGNED_INTEGER,
  EXPR_INFO_FLOAT,
  EXPR_INFO_DOUBLE,
  EXPR_INFO_STRING,
};

struct Identifier
{
  IdentInfo Info = IDENT_INFO_NONE;
  TypeInfo Type = TYPE_INFO_NONE;
  std::string Name = "";
  long unsigned Index = 0;
};

struct Expression
{
  ExprInfo Info = EXPR_INFO_NONE;
  std::list<Expression> Arguments = {};

  Identifier IdentifierValue = {};
  long signed SignedIntegerValue = 0;
  long unsigned UnsignedIntegerValue = 0;
  float FloatValue = 0.0F;
  double DoubleValue = 0.0;
  std::string StringValue = "";

  template<typename ... T>
  Expression(ExprInfo i, T&& ... args) : Info(i), Arguments{ std::forward<T>(args) ... } {}

  Expression()                    : Info(EXPR_INFO_NONE) {}
  Expression(Identifier const& v) : Info(EXPR_INFO_IDENT), IdentifierValue(v) {}
  Expression(Identifier&& v)      : Info(EXPR_INFO_IDENT), IdentifierValue(std::move(v)) {}
  Expression(long signed v)       : Info(EXPR_INFO_SIGNED_INTEGER), SignedIntegerValue(v) {}
  Expression(long unsigned v)     : Info(EXPR_INFO_UNSIGNED_INTEGER), UnsignedIntegerValue(v) {}
  Expression(float v)             : Info(EXPR_INFO_FLOAT), FloatValue(v) {}
  Expression(double v)            : Info(EXPR_INFO_DOUBLE), DoubleValue(v) {}
  Expression(std::string&& v)     : Info(EXPR_INFO_STRING), StringValue(std::move(v)) {}

  Expression operator %= (Expression&& e) && { return Expression(EXPR_INFO_COPY, std::move(e), std::move(*this)); }
};

template<typename ... T> inline Expression ExpressionNone(T&& ... args)           { return Expression(EXPR_INFO_NONE,             std::forward<T>(args)...); }
template<typename ... T> inline Expression ExpressionIdent(T&& ... args)          { return Expression(EXPR_INFO_IDENT,            std::forward<T>(args)...); }
template<typename ... T> inline Expression ExpressionSignedInteger(T&& ... args)  { return Expression(EXPR_INFO_SIGNED_INTEGER,   std::forward<T>(args)...); }
template<typename ... T> inline Expression ExpressionUnignedInteger(T&& ... args) { return Expression(EXPR_INFO_UNSIGNED_INTEGER, std::forward<T>(args)...); }
template<typename ... T> inline Expression ExpressionFloat(T&& ... args)          { return Expression(EXPR_INFO_FLOAT,            std::forward<T>(args)...); }
template<typename ... T> inline Expression ExpressionDouble(T&& ... args)         { return Expression(EXPR_INFO_DOUBLE,           std::forward<T>(args)...); }
template<typename ... T> inline Expression ExpressionString(T&& ... args)         { return Expression(EXPR_INFO_STRING,           std::forward<T>(args)...); }
template<typename ... T> inline Expression ExpressionComma(T&& ... args)          { return Expression(EXPR_INFO_COMMA,            std::forward<T>(args)...); }

struct Function
{
  std::string Name = "";
  long unsigned ArgCount = 0;
  long unsigned VarCount = 0;
  Expression Code = {};
};

struct Context;

} // %code

%param { Context& context }

%code
{

struct Context
{
  yy::location Location;

  char const* Cursor = nullptr;
  char const* Marker = nullptr;

  std::vector<std::map<std::string, Identifier>> Scopes = {};
  std::vector<Function> Functions = {};

  Function function = {};

  Identifier const& Define(std::string const& n, Identifier&& i)
  {
      auto const [it, inserted] = Scopes.back().emplace(n, std::move(i));

      if (!inserted)
      {
        throw yy::easy_parser::syntax_error(Location, "Duplicate definition <" + n + ">");
      }

      return it->second;
  }

  Expression DefineFunction(TypeInfo t, std::string const& n) { return Define(n, Identifier{ IDENT_INFO_FUNC, t, n, (long unsigned)Functions.size() }); }
  Expression DefineArgument(TypeInfo t, std::string const& n) { return Define(n, Identifier{ IDENT_INFO_ARG, t, n, function.ArgCount++ }); }
  Expression DefineVariable(TypeInfo t, std::string const& n) { return Define(n, Identifier{ IDENT_INFO_VAR, t, n, function.VarCount++ }); }

  Expression Use(std::string const& n)
  {
      for (auto j = Scopes.crbegin(); j != Scopes.crend(); ++j)
      {
        if (auto i = j->find(n); i != j->cend())
        {
          return i->second;
        }
      }

      throw yy::easy_parser::syntax_error(Location, "Undefined identifier <" + n + ">");

      return ExpressionNone();
  }

  void AddFunction(std::string&& n, Expression&& e)
  {
    function.Name = std::move(n);
    function.Code = std::move(e); // TODO

    Functions.push_back(std::move(function));

    function = {};
  }

  void operator ++ () { Scopes.emplace_back(); }
  void operator -- () { Scopes.pop_back(); }
};

namespace yy { easy_parser::symbol_type yylex(Context& context); }

#define M(X) std::move(X)
#define C(X) Expression(X)

} // %code

%token END 0

%token IDENTIFIER
%token SIGNED_INTEGER
%token UNSIGNED_INTEGER
%token FLOAT
%token DOUBLE
%token STRING

%token I8_TYPE  "i8"  I16_TYPE "i16" I32_TYPE "i32" I64_TYPE "i64"
%token U8_TYPE  "u8"  U16_TYPE "u16" U32_TYPE "u32" U64_TYPE "u64"
%token R32_TYPE "r32" R64_TYPE "r64"

%type<std::string>   IDENTIFIER
%type<long signed>   SIGNED_INTEGER
%type<long unsigned> UNSIGNED_INTEGER
%type<float>         FLOAT
%type<double>        DOUBLE
%type<std::string>   STRING

%type<std::string> I8_TYPE  I16_TYPE I32_TYPE I64_TYPE
%type<std::string> U8_TYPE  U16_TYPE U32_TYPE U64_TYPE
%type<std::string> R32_TYPE R64_TYPE

%type<TypeInfo>   type
%type<Expression> expression statement compound_statement variable_definition

%%

library: { ++context; } function { --context; }
       ;

function: function type IDENTIFIER { context.DefineFunction($2, $3); ++context; } '(' argument ')' statement { context.AddFunction(M($3), M($8)); --context; }
        | %empty                   { }
        ;

argument: argument ',' type IDENTIFIER { context.DefineArgument($3, $4); }
        | type IDENTIFIER              { context.DefineArgument($1, $2); }
        | %empty
        ;

statement: compound_statement '}'
         | expression ';'
         ;

compound_statement: '{'                          { $$ = ExpressionComma(); ++context; }
                  | compound_statement statement { $$ = M($1); $$.Arguments.push_back(M($2)); }
                  ;

type: I8_TYPE  { $$ = TYPE_INFO_I8; }
    | I16_TYPE { $$ = TYPE_INFO_I16; }
    | I32_TYPE { $$ = TYPE_INFO_I32; }
    | I64_TYPE { $$ = TYPE_INFO_I64; }
    | U8_TYPE  { $$ = TYPE_INFO_U8; }
    | U16_TYPE { $$ = TYPE_INFO_U16; }
    | U32_TYPE { $$ = TYPE_INFO_U32; }
    | U64_TYPE { $$ = TYPE_INFO_U64; }
    | R32_TYPE { $$ = TYPE_INFO_R32; }
    | R64_TYPE { $$ = TYPE_INFO_R64; }
    ;

expression: IDENTIFIER          { $$ = context.Use($1); }
          | SIGNED_INTEGER      { $$ = $1; }
          | UNSIGNED_INTEGER    { $$ = $1; }
          | FLOAT               { $$ = $1; }
          | DOUBLE              { $$ = $1; }
          | STRING              { $$ = M($1); }
          | variable_definition { $$ = M($1); }
          ;

variable_definition: type IDENTIFIER '=' expression { context.DefineVariable($1, $2) %= M($4); }
                   ;

%%

yy::easy_parser::symbol_type yy::yylex(Context& context)
{
  char const* anchor = context.Cursor;

  context.Location.step();

  auto s = [&](auto f, auto&& ... args) { context.Location.columns(context.Cursor - anchor); return f(args ..., context.Location); };

%{ // %re2c
re2c:yyfill:enable   = 0;

re2c:define:YYCTYPE  = char;
re2c:define:YYCURSOR = context.Cursor;
re2c:define:YYMARKER = context.Marker;

digit        = [0-9];
hex_digit    = [0-9a-fA-F];
oct_digit    = [0-7];
bin_digit    = [01];
exp          = [eE][-+]?{digit}+;
signedness   = [uU];
float_suffix = [fF];

"i8"                                     { return s(easy_parser::make_I8_TYPE, std::string(anchor, context.Cursor)); }
"i16"                                    { return s(easy_parser::make_I16_TYPE, std::string(anchor, context.Cursor)); }
"i32"                                    { return s(easy_parser::make_I32_TYPE, std::string(anchor, context.Cursor)); }
"i64"                                    { return s(easy_parser::make_I64_TYPE, std::string(anchor, context.Cursor)); }

"u8"                                     { return s(easy_parser::make_U8_TYPE, std::string(anchor, context.Cursor)); }
"u16"                                    { return s(easy_parser::make_U16_TYPE, std::string(anchor, context.Cursor)); }
"u32"                                    { return s(easy_parser::make_U32_TYPE, std::string(anchor, context.Cursor)); }
"u64"                                    { return s(easy_parser::make_U64_TYPE, std::string(anchor, context.Cursor)); }

"r32"                                    { return s(easy_parser::make_R32_TYPE, std::string(anchor, context.Cursor)); }
"r64"                                    { return s(easy_parser::make_R64_TYPE, std::string(anchor, context.Cursor)); }

[a-zA-Z_] [a-zA-Z_0-9]*                  { return s(easy_parser::make_IDENTIFIER, std::string(anchor, context.Cursor)); }

{digit}+                                 { return s(easy_parser::make_SIGNED_INTEGER, std::strtoll(anchor, nullptr, 10)); }
{digit}+{signedness}                     { return s(easy_parser::make_UNSIGNED_INTEGER, std::strtoull(anchor, nullptr, 10)); }

"0x"{hex_digit}+                         { return s(easy_parser::make_SIGNED_INTEGER, std::strtoll(anchor + 2, nullptr, 16)); }
"0x"{hex_digit}+{signedness}             { return s(easy_parser::make_UNSIGNED_INTEGER, std::strtoull(anchor + 2, nullptr, 16)); }

"0o"{oct_digit}+                         { return s(easy_parser::make_SIGNED_INTEGER, std::strtoll(anchor + 2, nullptr, 8)); }
"0o"{oct_digit}+{signedness}             { return s(easy_parser::make_UNSIGNED_INTEGER, std::strtoull(anchor + 2, nullptr, 8)); }

"0b"{bin_digit}+                         { return s(easy_parser::make_SIGNED_INTEGER, std::strtoll(anchor + 2, nullptr, 2)); }
"0b"{bin_digit}+{signedness}             { return s(easy_parser::make_UNSIGNED_INTEGER, std::strtoull(anchor + 2, nullptr, 2)); }

{digit}+"."{digit}*{exp}?                { return s(easy_parser::make_DOUBLE, std::strtod(anchor, nullptr)); }
{digit}+"."{digit}*{exp}?{float_suffix}? { return s(easy_parser::make_FLOAT, std::strtof(anchor, nullptr)); }

"\"" [^"]* "\""                          { return s(easy_parser::make_STRING, std::string(anchor + 1, context.Cursor - 1)); }

"\000"                                   { return s(easy_parser::make_END); }
"\r\n" | [\r\n]                          { context.Location.lines();   return yylex(context); }
"//" [^\r\n]*                            {                             return yylex(context); }
[\t\v\b\f ]                              { context.Location.columns(); return yylex(context); }

.                                        { return s([](auto ... s) { return easy_parser::symbol_type(s ...); }, easy_parser::token_type(context.Cursor[-1] & 0xFF)); }
%} // %re2c

  return yylex(context);
}

void yy::easy_parser::error(location_type const& l, std::string const& m)
{
  std::printf("%s:%u:%u-%u: %s\n",
    (l.begin.filename ? l.begin.filename->data() : "(undefined)"),
    l.begin.line,
    l.begin.column,
    l.end.column,
    m.data());
}

#include <fstream>

void stringify()
{
  
}

int main(int argc, char** argv)
{
  std::string filename = argv[1];
  std::ifstream f(filename);
  std::string buffer(std::istreambuf_iterator<char>(f), {});

  Context context;
  context.Cursor = buffer.data();
  context.Location.begin.filename = &filename;
  context.Location.end.filename   = &filename;

  yy::easy_parser parser(context);
  parser.parse();

  std::vector<Function> functions = std::move(context.Functions);

  stringify();
}