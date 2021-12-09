#include <unordered_set>
#include <stdexcept>
#include "lexer.hpp"

bool IsNumber(char x) {
  return x >= '0' && x <= '9';
}

std::unordered_set<char> special_symbols = {';', '{', '}', '(', ')', '.', ','};

bool IsSpecialSymbol(char x) {
  return special_symbols.find(x) != special_symbols.end();
}

std::unordered_set<char> break_symbols = {'+', '-', '*', '/', '=', '!', '<', '>'};

bool IsBreakSymbol(char x) {
  return break_symbols.find(x) != special_symbols.end();
}

void Lexer::ParseText(std::string&& text) {
  using Type = LexerToken::Type;
  for (char x: text) {
    if (x == '\"') {
      if (current_state != Type::StringLiteral) {
        current_info += x;
        PushToken();
        current_state = Type::StringLiteral;
      } else {
        current_info += x;
        PushToken();
      }
    } else if (current_state == Type::StringLiteral) {
      current_info += x;
    } else if (std::isspace(x)) {
      PushToken();
    } else if (IsBreakSymbol(x)) {
      if (x == '=') {
        if (current_state == Type::Plus) {
          current_state = Type::PlusEqual;
          PushToken();
        } else if (current_state == Type::Minus) {
          current_state = Type::MinusEqual;
          PushToken();
        } else if (current_state == Type::Star) {
          current_state = Type::StarEqual;
          PushToken();
        } else if (current_state == Type::Slash) {
          current_state = Type::SlashEqual;
          PushToken();
        } else if (current_state == Type::Equal) {
          current_state = Type::EqualEqual;
          PushToken();
        } else if (current_state == Type::ExclamationMark) {
          current_state = Type::ExclamationMarkEqual;
          PushToken();
        } else if (current_state == Type::LAngle) {
          current_state = Type::LAngleEqual;
          PushToken();
        } else if (current_state == Type::RAngle) {
          current_state = Type::RAngleEqual;
          PushToken();
        } else {
          PushToken();
          current_state = Type::Equal;
        }
      } else if (x == '+') {
        if (current_state == Type::Plus) {
          current_state = Type::PlusPlus;
          PushToken();
        } else {
          PushToken();
          current_state = Type::Plus;
        }
      } else if (x == '-') {
        if (current_state == Type::Minus) {
          current_state = Type::MinusMinus;
          PushToken();
        } else if (current_state == Type::LAngle) {
          current_state = Type::LArrow;
          PushToken();
        } else {
          PushToken();
          current_state = Type::Minus;
        }
      } else if (x == '>') {
        if (current_state == Type::Minus) {
          current_state = Type::RArrow;
          PushToken();
        } else {
          PushToken();
          current_state = Type::RAngle;
        }
      } else {
        PushToken();
        if (x == '+') {
          current_state = Type::Plus;
        } else if (x == '-') {
          current_state = Type::Minus;
        } else if (x == '*') {
          current_state = Type::Star;
        } else if (x == '/') {
          current_state = Type::Slash;
        } else if (x == '=') {
          current_state = Type::Equal;
        } else if (x == '!') {
          current_state = Type::ExclamationMark;
        } else if (x == '<') {
          current_state = Type::LAngle;
        } else if (x == '>') {
          current_state = Type::RAngle;
        } else {
          throw std::logic_error("incorrect operator");
        }
      }
    } else if (current_state == Type::None || IsSpecialSymbol(x)) {
      PushToken();
      if (x == ';') {
        current_state = Type::Semicolon;
        PushToken();
      } else if (x == '{') {
        current_state = Type::CurlyOpenBracket;
        PushToken();
      } else if (x == '}') {
        current_state = Type::CurlyCloseBracket;
        PushToken();
      } else if (x == '(') {
        current_state = Type::RoundOpenBracket;
        PushToken();
      } else if (x == ')') {
        current_state = Type::RoundCloseBracket;
        PushToken();
      } else if (x == '.') {
        current_state = Type::Period;
        PushToken();
      } else if (x == ',') {
        current_state = Type::Commo;
        PushToken();
      } else {
        current_state = Type::Word;
        current_info += x;
      }
    } else if (current_state == Type::Word) {
      current_info += x;
    }
  }

  PushToken();
}

void Lexer::ParseText(const std::string& text) {
  std::string copy(text);
  ParseText(std::move(copy));
}

void Lexer::PushToken() {
  using Type = LexerToken::Type;
  if (current_state != Type::None) {
    if (current_state == Type::Word && current_info == "for") {
      current_state = Type::FOR;
      current_info.clear();
    } else if (current_state == Type::Word && current_info == "while") {
      current_state = Type::WHILE;
      current_info.clear();
    } else if (current_state == Type::Word && current_info == "if") {
      current_state = Type::IF;
      current_info.clear();
    } else if (current_state == Type::Word && current_info == "else") {
      current_state = Type::ELSE;
      current_info.clear();
    }
    tokens.emplace_back(current_state, std::move(current_info));
    current_state = Type::None;
    current_info.clear();
  }
}

LexerTokenList Lexer::GetTokens() const {
  return tokens;
}

namespace std {
std::string to_string(Lexer::LexerToken::Type x) {
  using Type = Lexer::LexerToken::Type;
  if (x == Type::None) {
    return "None";
  } else if (x == Type::Word) {
    return "Word";
  } else if (x == Type::Semicolon) {
    return "Semicolon";
  } else if (x == Type::CurlyOpenBracket) {
    return "CurlyOpenBrackets";
  } else if (x == Type::CurlyCloseBracket) {
    return "CurlyCloseBrackets";
  } else if (x == Type::RoundOpenBracket) {
    return "RoundOpenBrackets";
  } else if (x == Type::RoundCloseBracket) {
    return "RoundCloseBrackets";
  } else if (x == Type::Commo) {
    return "Commo";
  } else if (x == Type::Plus) {
    return "Plus";
  } else if (x == Type::Minus) {
    return "Minus";
  } else if (x == Type::Star) {
    return "Star";
  } else if (x == Type::Slash) {
    return "Slash";
  } else if (x == Type::Equal) {
    return "Equal";
  } else if (x == Type::ExclamationMark) {
    return "ExclamationMark";
  } else if (x == Type::PlusEqual) {
    return "PlusEqual";
  } else if (x == Type::MinusEqual) {
    return "MinusEqual";
  } else if (x == Type::StarEqual) {
    return "StarEqual";
  } else if (x == Type::SlashEqual) {
    return "SlashEqual";
  } else if (x == Type::EqualEqual) {
    return "EqualEqual";
  } else if (x == Type::ExclamationMarkEqual) {
    return "ExclamationMarkEqual";
  } else if (x == Type::PlusPlus) {
    return "PlusPlus";
  } else if (x == Type::MinusMinus) {
    return "MinusMinus";
  } else if (x == Type::LAngle) {
    return "LAngle";
  } else if (x == Type::RAngle) {
    return "RAngle";
  } else if (x == Type::LAngleEqual) {
    return "LAngleEqual";
  } else if (x == Type::RAngleEqual) {
    return "RAngleEqual";
  } else if (x == Type::LArrow) {
    return "LArrow";
  } else if (x == Type::RArrow) {
    return "RArrow";
  } else if (x == Type::FOR) {
    return "for";
  } else if (x == Type::WHILE) {
    return "while";
  } else if (x == Type::IF) {
    return "if";
  } else if (x == Type::ELSE) {
    return "else";
  } else {
    return "ERROR";
  }
}
}
