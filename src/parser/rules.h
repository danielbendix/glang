#ifndef PARSER_RULES_H
#define PARSER_RULES_H

#include "token.h"
#include "parser.h"

#include <algorithm>

enum class Precedence : int {
    None,
    LogicalOr,      // or
    LogicalAnd,     // and
    LogicalNot,     // not
    Equality,       // == !=
    Comparison,     // < > <= >=
    Shift,          // << >>
    Range,          // ... ..<
    Term,           // + -
    Factor,         // * /
    // FIXME: Ensure bit operators have the desired precedence
    BitwiseAnd,     // &
    BitwiseXor,     // ^
    BitwiseOr,      // |
    Unary,          // ! -
    Call,           // . () []
    Stop,
};

inline Precedence operator++(Precedence& precedence) {
    precedence = Precedence{static_cast<int>(precedence) + 1};
    assert(precedence <= Precedence::Stop);
    return precedence;
}

inline bool operator<=(Precedence l, Precedence r)
{
    return static_cast<int>(l) <= static_cast<int>(r);
}

inline Precedence operator+(Precedence l, int i)
{
    return static_cast<Precedence>(static_cast<int>(l) + i);
}

struct ParseRule {
    using ExpressionPrefixHandler = AST::Expression *(Parser::*)();
    using ExpressionInfixHandler = AST::Expression *(Parser::*)(AST::Expression *);

    ExpressionPrefixHandler prefixHandler;
    ExpressionInfixHandler infixHandler;
    Precedence precedence;

    using enum TokenType;
    static ParseRule expressionRules[];
};

namespace CompileTime {
    using PrefixHandler = ParseRule::ExpressionPrefixHandler;
    using InfixHandler = ParseRule::ExpressionInfixHandler;
    using PrecedenceLevel = Precedence;

    using IndexType = std::underlying_type_t<TokenType>;
    
    template<typename T>
    consteval bool checkContiguousPairs(IndexType array[], std::integer_sequence<T> /*unused*/) {
        return true;
    }

    template <typename T, T head, T... tail>
    consteval bool checkContiguousPairs(IndexType array[], std::integer_sequence<T, head, tail...> /*unused*/) {
        return (array[head] + 1 == array[head + 1]) && checkContiguousPairs(array, std::integer_sequence<T, tail...>{});
    }

    template <typename T, T Min, T Max, T... Values>
    requires std::is_integral_v<T>
    consteval bool isDenseUniqueRange(std::integer_sequence<T, Values...> /* unused */) {
        constexpr std::size_t size = sizeof...(Values);
        static_assert(size == Max + 1);

        using Indices = std::make_integer_sequence<std::size_t, size - 1>;
        
        if (size == 0) {
            return false;
        }

        T values[] = {Values...};

        std::sort(std::begin(values), std::end(values));

        if (values[0] != Min) {
            return false;
        }
        if (values[size - 1] != Max) {
            return false;
        }

        return checkContiguousPairs(values, Indices{});
    }

    template <TokenType I, PrefixHandler Prefix, InfixHandler Infix, PrecedenceLevel Precedence>
    struct ParseRule_ {
        constexpr static inline TokenType Index = I;
        __attribute__((always_inline))
        static void populateTable(ParseRule *table) {
            table[static_cast<IndexType>(Index)] = ParseRule{Prefix, Infix, Precedence};
        }
    };

    template <TokenType Index>
    using NoRule = ParseRule_<Index, nullptr, nullptr, PrecedenceLevel::None>;

    template <TokenType... Indices>
    struct TokenTypeList {
        consteval TokenTypeList() = default;
        using IndexSequence = std::integer_sequence<IndexType, IndexType(Indices)...>;
    };

    template <TokenType... TokenTypes>
    consteval bool isDenseUniqueRange(TokenTypeList<TokenTypes...> /* unused */) {
        return isDenseUniqueRange<IndexType, 0, static_cast<IndexType>(TokenType::COUNT) - 1>(typename TokenTypeList<TokenTypes...>::IndexSequence{});
    }

    template <typename... Rules>
    struct RuleList {
        using Indices = TokenTypeList<Rules::Index...>;
        using RawIndices = std::integer_sequence<IndexType, static_cast<IndexType>(Rules::Index)...>;

        __attribute__((always_inline))
        static void populateTable(ParseRule *table) {
            (Rules::populateTable(table), ...);
        }

        static_assert(isDenseUniqueRange(Indices{}));
    };
}

namespace ParserInternals {
    using namespace CompileTime;
    using enum TokenType;
    struct Rules {
        using Rules_ = RuleList<
            NoRule<Empty>,
            ParseRule_<LeftBrace, nullptr, &Parser::subscript, Precedence::Call>,
            ParseRule_<LeftParenthesis, &Parser::grouping, &Parser::call, Precedence::Call>,
            ParseRule_<LeftBracket, &Parser::inferredInitializer, nullptr, Precedence::None>,

            NoRule<RightBrace>,
            NoRule<RightParenthesis>,
            NoRule<RightBracket>,

            NoRule<Comma>,
            NoRule<Colon>,
            NoRule<Semicolon>,
            NoRule<Arrow>,
            NoRule<Question>,
            NoRule<DotDot>,

            ParseRule_<Dot, &Parser::inferredMember, &Parser::member, Precedence::Call>,

            ParseRule_<DotDotDot, nullptr, &Parser::binary, Precedence::Range>,
            ParseRule_<DotDotLess, nullptr, &Parser::binary, Precedence::Range>,

            ParseRule_<Bang, nullptr, &Parser::postfixUnary, Precedence::Unary>,
            ParseRule_<At, nullptr, &Parser::postfixUnary, Precedence::Unary>,

            ParseRule_<Plus, nullptr, &Parser::binary, Precedence::Term>,
            ParseRule_<Minus, &Parser::prefixUnary, &Parser::binary, Precedence::Term>,
            ParseRule_<Star, &Parser::prefixUnary, &Parser::binary, Precedence::Factor>,
            ParseRule_<Slash, nullptr, &Parser::binary, Precedence::Factor>,
            ParseRule_<Percent, nullptr, &Parser::binary, Precedence::Factor>,

            ParseRule_<LessLess, nullptr, &Parser::binary, Precedence::Shift>,
            ParseRule_<GreaterGreater, nullptr, &Parser::binary, Precedence::Shift>,

            ParseRule_<Tilde, &Parser::prefixUnary, nullptr, Precedence::None>,
            ParseRule_<Ampersand, &Parser::prefixUnary, &Parser::binary, Precedence::BitwiseAnd>,
            ParseRule_<Caret, nullptr, &Parser::binary, Precedence::BitwiseXor>,
            ParseRule_<Pipe, nullptr, &Parser::binary, Precedence::BitwiseOr>,

            ParseRule_<Not, &Parser::prefixUnary, nullptr, Precedence::None>,
            ParseRule_<And, nullptr, &Parser::binary, Precedence::LogicalAnd>,
            ParseRule_<Or, nullptr, &Parser::binary, Precedence::LogicalOr>,

            ParseRule_<BangEqual, nullptr, &Parser::binary, Precedence::Equality>,
            ParseRule_<EqualEqual, nullptr, &Parser::binary, Precedence::Equality>,

            ParseRule_<Greater, nullptr, &Parser::binary, Precedence::Comparison>,
            ParseRule_<GreaterEqual, nullptr, &Parser::binary, Precedence::Comparison>,
            ParseRule_<Less, nullptr, &Parser::binary, Precedence::Comparison>,
            ParseRule_<LessEqual, nullptr, &Parser::binary, Precedence::Comparison>,


            ParseRule_<Nil, &Parser::literal, nullptr, Precedence::None>,
            ParseRule_<True, &Parser::literal, nullptr, Precedence::None>,
            ParseRule_<False, &Parser::literal, nullptr, Precedence::None>,
            ParseRule_<Integer, &Parser::literal, nullptr, Precedence::None>,
            ParseRule_<Binary, &Parser::literal, nullptr, Precedence::None>,
            ParseRule_<Octal, &Parser::literal, nullptr, Precedence::None>,
            ParseRule_<Hexadecimal, &Parser::literal, nullptr, Precedence::None>,
            ParseRule_<Floating, &Parser::literal, nullptr, Precedence::None>,
            ParseRule_<Character, &Parser::literal, nullptr, Precedence::None>,
            ParseRule_<String, &Parser::literal, nullptr, Precedence::None>,

            ParseRule_<Identifier, &Parser::identifier, nullptr, Precedence::None>,
            ParseRule_<HashIdentifier, &Parser::intrinsic, nullptr, Precedence::None>,
            ParseRule_<Self, &Parser::self, nullptr, Precedence::None>,

            NoRule<Equal>, 
            NoRule<PlusEqual>, 
            NoRule<MinusEqual>,
            NoRule<StarEqual>, 
            NoRule<SlashEqual>,
            NoRule<PercentEqual>,

            NoRule<AmpersandEqual>,
            NoRule<CaretEqual>,
            NoRule<PipeEqual>,

            NoRule<Enum>,
            NoRule<Struct>,

            NoRule<Var>,
            NoRule<Const>,
            NoRule<Bind>,
            NoRule<Unwrap>,

            NoRule<Static>,
            NoRule<Public>,
            NoRule<Private>,
            NoRule<Unpadded>,
            NoRule<Compact>,

            NoRule<Case>,
            NoRule<If>,
            NoRule<Else>,
            NoRule<Init>,
            NoRule<Fn>,
            NoRule<For>,
            NoRule<In>,
            NoRule<Guard>,
            NoRule<While>,
            NoRule<Where>,
            NoRule<Repeat>,
            NoRule<Break>,
            NoRule<Continue>,
            NoRule<Return>,

            NoRule<Try>,
            NoRule<TryQuestion>,
            NoRule<TryBang>,
            NoRule<Throw>,

            NoRule<Error>,
            NoRule<EndOfFile>
        >;
    };
}

struct ParserRules {
    ParseRule rules[static_cast<std::size_t>(TokenType::COUNT)];

    ParserRules() {
        ParserInternals::Rules::Rules_::populateTable(rules);
    }
} const parserTable;

#include <optional>

namespace Test {
    enum class ToWrap {
        A,
        B,
    };

    struct Wrapper {
        ToWrap w;

        consteval Wrapper(ToWrap w) : w{w} {}
    };

    struct RuleBuilder {

        TokenType index;
        std::optional<CompileTime::PrefixHandler> prefix;
        std::optional<CompileTime::InfixHandler> infix;
        std::optional<CompileTime::PrecedenceLevel> precedence;
    };



    template <ToWrap A>
    struct Test {};

    
    template <typename T>
    consteval Wrapper operator<<(ToWrap t, T a) {
        return Wrapper{t};
    }

    template <Wrapper W>
    struct AsTemplate {
        using Type = Test<W.w>;
    };

    int test() {
        ToWrap::A << 1;
    }

    template <typename... Types>
    struct List {};

    template <template <typename...> class List, auto... Values> 
    struct GetTypes {
        using Type = List<decltype(Values)...>;
    };

    using A = typename GetTypes<List, 'c', 1, 1U, 1L, 1LL>::Type;
    static_assert(std::is_same_v<A, List<char, int, unsigned, long, long long>>);


    template <Wrapper... Ws>
    struct ListThing {
        using Type = List<typename AsTemplate<Ws>::Type...>;
    };

    using T = ListThing<ToWrap::A << 1>::Type;
    static_assert(std::is_same_v<T, List<Test<ToWrap::A>>>);



}

#endif
