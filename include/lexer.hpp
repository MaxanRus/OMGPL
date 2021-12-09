#pragma once
#include <string>
#include <list>

class Lexer {
 public:
  struct LexerToken {
    enum class Type {
      None, Word, Semicolon, CurlyOpenBracket, CurlyCloseBracket,
      RoundOpenBracket, RoundCloseBracket, Period, Commo, Plus, Minus, Star,
      Slash, Equal, ExclamationMark, PlusEqual, MinusEqual, StarEqual,
      SlashEqual, EqualEqual, ExclamationMarkEqual, PlusPlus, MinusMinus,
      LAngle, RAngle, LAngleEqual, RAngleEqual, LArrow, RArrow, FOR, IF, ELSE,
      WHILE, StringLiteral
    };
    LexerToken() = default;
    LexerToken(const LexerToken&) = default;
    LexerToken(LexerToken&&) = default;
    LexerToken(const Type& type, const std::string& info) : type(type), info(info) {}
    LexerToken(const Type& type, std::string&& info) : type(type), info(info) {}
    LexerToken(Type&& type, const std::string& info) : type(type), info(info) {}
    LexerToken(Type&& type, std::string&& info) : type(type), info(info) {}

    LexerToken& operator=(LexerToken&&) = default;
    LexerToken& operator=(const LexerToken&) = default;

    Type type;
    std::string info;
  };
  using LexerTokenList = std::list<LexerToken>;

  void ParseText(std::string&&);
  void ParseText(const std::string&);

  LexerTokenList GetTokens() const;
 private:
  void PushToken();
  LexerTokenList tokens;

  LexerToken::Type current_state = LexerToken::Type::None;
  std::string current_info;
};

using LexerTokenList = Lexer::LexerTokenList;

namespace std {
std::string to_string(Lexer::LexerToken::Type);
}
