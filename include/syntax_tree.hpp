#pragma once
#include <variant>
#include <limits>
#include <vector>
#include <memory>
#include "lexer.hpp"

struct Node;
struct Variable;
struct VariableInStack;
struct Expression;

struct Node {
  Node* parent = nullptr;
  Node* next = nullptr;
  Node* previous = nullptr;

  void Insert(Node*);
  void InsertBefore(Node*);
  virtual void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) = 0;
  virtual ~Node() {}
};

enum class Operation {
  None,
  Plus,
  Minus,
  Star,
  Slash,
  Equal,
  ExclamationMark,
  PlusEqual,
  MinusEqual,
  StarEqual,
  SlashEqual,
  EqualEqual,
  ExclamationMarkEqual,
  PlusPlus,
  MinusMinus,
  LAngle,
  RAngle,
  LAngleEqual,
  RAngleEqual,
  Value
};

enum class PriorityType {
  L, R
};

struct TypeVariable {
  enum ID {
    none = 0, type_int = 1, type_string = 2
  };

  TypeVariable() {}
  TypeVariable(ID id) : id(id) {}
  ID id = ID::none;
  size_t size;
};

struct DeallocateStack : public Node {
  DeallocateStack(size_t count) : count(count) {}
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override {
    for (size_t i = 0; i < count; ++i) {
      stack.pop_back();
    }
  }
  size_t count;
};

struct HiddenDeallocateStack : public Node {
  HiddenDeallocateStack(size_t count) : count(count) {}
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override {
    for (size_t i = 0; i < count; ++i) {
      stack.pop_back();
    }
  }
  size_t count;
};

struct VariableInStack {
  VariableInStack(TypeVariable type_variable) : type_variable(type_variable) {}
  void CallOperator(std::shared_ptr<VariableInStack>& another, std::shared_ptr<VariableInStack>& result, Operation op) {
    if (op == Operation::Equal || op == Operation::PlusEqual || op == Operation::MinusEqual || op == Operation::StarEqual || op == Operation::SlashEqual) {
      if (type_variable.id != TypeVariable::ID::none && type_variable.id != another->type_variable.id) {
        throw std::logic_error("error variable convert");
      }
      if (result->type_variable.id != TypeVariable::ID::none && result->type_variable.id != another->type_variable.id) {
        throw std::logic_error("error variable convert");
      }
      result->Clear();
      result->type_variable.id = another->type_variable.id;
      result->Allocate();
      if (op == Operation::Equal) {
        Clear();
        type_variable.id = another->type_variable.id;
        Allocate();
      }
      if (another->type_variable.id == TypeVariable::ID::type_int) {
        int& x = *static_cast<int*>(memory);
        int& y = *static_cast<int*>(another->memory);
        int& res = *static_cast<int*>(result->memory);
        if (op == Operation::Equal) {
          if (this == another.get()) {
            res = x;
          } else {
            res = x = y;
          }
        } else if (op == Operation::PlusEqual) {
          if (this == another.get()) {
            res += x;
          } else {
            res = x += y;
          }
        } else if (op == Operation::MinusEqual) {
          if (this == another.get()) {
            res -= x;
          } else {
            res = x -= y;
          }
        } else if (op == Operation::StarEqual) {
          if (this == another.get()) {
            res *= x;
          } else {
            res = x *= y;
          }
        } else {
          if (this == another.get()) {
            res /= x;
          } else {
            res = x /= y;
          }
        }
      } else if (another->type_variable.id == TypeVariable::ID::type_string) {
        std::string& x = *static_cast<std::string*>(memory);
        std::string& y = *static_cast<std::string*>(another->memory);
        std::string& res = *static_cast<std::string*>(result->memory);
        if (op == Operation::Equal) {
          if (this == another.get()) {
            res = x;
          } else {
            res = x = y;
          }
        } else if (op == Operation::PlusEqual) {
          if (this == another.get()) {
            res += x;
          } else {
            res = x += y;
          }
        } else {
          throw std::logic_error("invalid operation string");
        }
      }
      return;
    } else if (op == Operation::EqualEqual || op == Operation::ExclamationMarkEqual || op == Operation::LAngle || op == Operation::RAngle || op == Operation::LAngleEqual || op == Operation::RAngleEqual) {
      result->Clear();
      result->type_variable.id = TypeVariable::ID::type_int;
      result->Allocate();
      int& res = *static_cast<int*>(result->memory);

      if (another->type_variable.id == TypeVariable::ID::type_int && type_variable.id == TypeVariable::ID::type_int) {
        auto& x = *static_cast<int*>(memory);
        auto& y = *static_cast<int*>(another->memory);
        if (op == Operation::EqualEqual) {
          res = (x == y);
        } else if (op == Operation::ExclamationMarkEqual) {
          res = (x != y);
        } else if (op == Operation::LAngle) {
          res = (x < y);
        } else if (op == Operation::RAngle) {
          res = (x > y);
        } else if (op == Operation::LAngleEqual) {
          res = (x <= y);
        } else if (op == Operation::RAngleEqual) {
          res = (x >= y);
        }
      } else if (another->type_variable.id == TypeVariable::ID::type_string && type_variable.id == TypeVariable::ID::type_string) {
        auto& x = *static_cast<std::string*>(memory);
        auto& y = *static_cast<std::string*>(another->memory);
        if (op == Operation::EqualEqual) {
          res = (x == y);
        } else if (op == Operation::ExclamationMarkEqual) {
          res = (x != y);
        } else if (op == Operation::LAngle) {
          res = (x < y);
        } else if (op == Operation::RAngle) {
          res = (x > y);
        } else if (op == Operation::LAngleEqual) {
          res = (x <= y);
        } else if (op == Operation::RAngleEqual) {
          res = (x >= y);
        }
      }
    } else if (op == Operation::Plus || op == Operation::Minus || op == Operation::Star || op == Operation::Slash) {
      result->Clear();
      result->type_variable.id = another->type_variable.id;
      result->Allocate();
      if (another->type_variable.id == TypeVariable::ID::type_int && type_variable.id == TypeVariable::ID::type_int) {
        int& x = *static_cast<int*>(memory);
        int& y = *static_cast<int*>(another->memory);
        int& res = *static_cast<int*>(result->memory);
        if (op == Operation::Plus) {
          res = (x + y);
        } else if (op == Operation::Minus) {
          res = (x - y);
        } else if (op == Operation::Star) {
          res = (x * y);
        } else if (op == Operation::Slash) {
          res = (x / y);
        }
      } else if (another->type_variable.id == TypeVariable::ID::type_string && type_variable.id == TypeVariable::ID::type_string) {
        std::string& x = *static_cast<std::string*>(memory);
        std::string& y = *static_cast<std::string*>(another->memory);
        std::string& res = *static_cast<std::string*>(result->memory);
        if (op == Operation::Plus) {
          res = (x + y);
        }
      }
    }
    return;






    if (type_variable.id == TypeVariable::ID::type_int) {
      int& x = *static_cast<int*>(memory);
      if (another->type_variable.id == TypeVariable::ID::type_int) {
        int& y = *static_cast<int*>(another->memory);
        result->type_variable.id = TypeVariable::ID::type_int;
        result->Clear();
        result->Allocate();
        int& res = *static_cast<int*>(result->memory);
        if (op == Operation::Equal) {
          /*
          if (this == &another)
            res = y;
          else
            x = res = y;
            */
        } else if (op == Operation::Plus) {
          res = x + y;
        } else if (op == Operation::Star) {
          res = x * y;
        } else if (op == Operation::LAngle) {
          res = x < y;
        }
      }
    } else if (type_variable.id == TypeVariable::ID::type_string) {
      using std::string;
      string& x = *static_cast<string*>(memory);
      if (another->type_variable.id == TypeVariable::ID::type_string) {
        string& y = *static_cast<string*>(another->memory);
        result->type_variable.id = TypeVariable::ID::type_string;
        result->Clear();
        result->Allocate();
        string& res = *static_cast<string*>(result->memory);
        if (op == Operation::Equal) {
          if (this == another.get())
            res = y;
          else
            x = res = y;
        }
      }
    }
  }
  void Clear() {
    if (memory) {
      if (type_variable.id == TypeVariable::ID::type_int) {
        delete static_cast<int*>(memory);
      } else if (type_variable.id == TypeVariable::ID::type_string) {
        delete static_cast<std::string*>(memory);
      }
    }
    memory = nullptr;
  }
  void Allocate() {
    if (type_variable.id == TypeVariable::ID::type_int) {
      memory = new int(15);
    } else if (type_variable.id == TypeVariable::ID::type_string) {
      memory = new std::string("lkajsdf");
    }
  }
  TypeVariable type_variable;
  void* memory = nullptr;
};

struct Variable : public Node {
  Variable(std::string type, std::string name) : type(std::move(type)), name(std::move(name)) {}
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override;
  std::string type;
  std::string name;
  Expression* default_value = nullptr;
  // TYPE
  // STORAGE
};

struct Container : public Node {
  void AddChildren(Node*);
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override {
    Node* cur = children_begin;
    while (cur) {
      cur->Run(stack);
      cur = cur->next;
    }
  }

  Node* children_begin = nullptr;
  Node* children_end = nullptr;
};

struct CodeBlock : public Container {};

struct Function : public Node {
  Function(const std::string& name) : name(name), code(new CodeBlock()) {}
  Function(std::string&& name) : name(name) {}
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override {
    code->Run(stack);
    // TODO
  }
  std::string name;
  CodeBlock* code;
  // RETURN VALUE
};

struct CreateVariables : Node {
  CreateVariables(size_t count) : count(count) {}
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override {}
  size_t count;
};

struct CallFunction : public Node {
  CallFunction(std::string name_function) : name_function(std::move(name_function)) {}
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override;
  std::string name_function;
  std::vector<Expression*> parameters;
};

using stack_pointer = size_t;

struct Expression : public Node {
  using Types = std::variant<std::string, Expression*, stack_pointer>;
  Expression() {}
  Expression(Types type1, Types type2, Operation op) : type1(std::move(type1)), type2(std::move(type2)), op(op) {
    if (op == Operation::Value) {
      count = 1;
    } else {
      count = 1;
      auto e_type1 = std::get<Expression*>(type1);
      auto e_type2 = std::get<Expression*>(type2);
      count += e_type1->count;
      count += e_type2->count;

      position_result = count;
      e_type2->AddStackPointer(e_type1->count);
    }
  }
  void AddStackPointer(stack_pointer d) {
    position_result += d;
  }
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override {
    if (op == Operation::Value) {
      if (std::holds_alternative<size_t>(type1)) {
        stack.push_back(*(stack.end() - std::get<size_t>(type1)));
      } else {
        stack.push_back(std::make_shared<VariableInStack>(TypeVariable()));
        if (std::get<std::string>(type1)[0] == '\"') {
          stack.back()->type_variable = TypeVariable(TypeVariable::ID::type_string);
          stack.back()->Allocate();
          (*(std::string*) stack.back()->memory) =
              std::get<std::string>(type1).substr(1, std::get<std::string>(type1).size() - 2);
        } else {
          stack.back()->type_variable = TypeVariable(TypeVariable::ID::type_int);
          stack.back()->Allocate();
          (*(int*) stack.back()->memory) = std::atoi(std::get<std::string>(type1).c_str());
        }
      }
    } else {
      std::get<Expression*>(type1)->Run(stack);
      std::shared_ptr<VariableInStack> x = std::move(stack.back());
      stack.pop_back();
      std::get<Expression*>(type2)->Run(stack);
      std::shared_ptr<VariableInStack> y = std::move(stack.back());
      stack.pop_back();
      stack.push_back(std::make_shared<VariableInStack>(TypeVariable()));
      x->CallOperator(y, stack.back(), op);
    }
  }
  static size_t GetPriority(Operation);
  static PriorityType GetPriorityType(Operation);
  static Operation Convert(Lexer::LexerToken::Type);
  Types type1, type2;
  Operation op = Operation::None;
  stack_pointer position_result = 0;
  size_t count;
};

struct FOR : public Node {
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override {
    while (true) {
      check->Run(stack);
      auto res = *((int*) stack.back()->memory);
      stack.pop_back();
      if (!res) {
        break;
      }
      code->Run(stack);
      size_t sz = stack.size();
      tick->Run(stack);
      while (stack.size() != sz) {
        stack.pop_back();
      }
    }
  }
  Variable* var = nullptr;
  Expression* check = nullptr;
  Expression* tick = nullptr;
  CodeBlock* code = nullptr;
};

struct IF : public Node {
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override {
    if (check) {
      check->Run(stack);
      auto res = *((int*) stack.back()->memory);
      stack.pop_back();
      if (res) {
        code->Run(stack);
      }
    }
  }
  Expression* check = nullptr;
  CodeBlock* code = nullptr;
};

struct WHILE : public Node {
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override {
    if (check) {
      while (true) {
        check->Run(stack);
        auto res = *((int*) stack.back()->memory);
        stack.pop_back();
        if (res) {
          code->Run(stack);
        } else {
          break;
        }
      }
    }
  }
  Expression* check = nullptr;
  CodeBlock* code = nullptr;
};

class SyntaxTree {
 public:
  SyntaxTree() {}
  void PushLexerTokenList(const LexerTokenList&);
  void Compile();
  void Run();
 private:
  bool IsTypeName(Node*, const std::string&);
  bool IsVariableName(Node*, const std::string&);
  bool IsFunctionName(Node*, const std::string&);
  CodeBlock* ParseCurlyBrackets(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushCurlyBrackets(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushRoundBrackets(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushFunction(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushLine(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  Variable* ParseNewVariable(const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushNewVariable(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushExpression(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushParametersFunction(CallFunction*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushCallFunction(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushFOR(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushSignatureFOR(FOR*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushIF(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushSignatureIF(IF*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushWHILE(Container*, const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushSignatureWHILE(WHILE*, const LexerTokenList&, LexerTokenList::const_iterator&);
  Expression* ParseExpression(LexerTokenList::const_iterator, LexerTokenList::const_iterator);
  Expression* ParseExpression(const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushDeallocateStack(Node*, size_t);
  stack_pointer GetCountStackOffsetForVariable(Node* node, std::string name);
  void LinkVariables(Node*);
  void LinkVariablesInExpression(Expression*, Node*);
  CodeBlock* tree_ = nullptr;
};

