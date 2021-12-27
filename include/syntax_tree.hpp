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

  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override;

  size_t count;
};

struct HiddenDeallocateStack : public Node {
  HiddenDeallocateStack(size_t count) : count(count) {}

  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override;

  size_t count;
};

struct VariableInStack {
  VariableInStack(TypeVariable type_variable) : type_variable(type_variable) {}

  void CallOperator(std::shared_ptr<VariableInStack>& another, std::shared_ptr<VariableInStack>& result, Operation op);

  void Clear();

  void Allocate();

  TypeVariable type_variable;
  void* memory = nullptr;
 private:

  void CallEqualOperator(const std::shared_ptr<VariableInStack>& another,
                         std::shared_ptr<VariableInStack>& result,
                         const Operation& op);
  void CallComparisonOperator(const std::shared_ptr<VariableInStack>& another,
                              std::shared_ptr<VariableInStack>& result,
                              const Operation& op) const;
  void CallArithmeticOperator(const std::shared_ptr<VariableInStack>& another,
                              std::shared_ptr<VariableInStack>& result,
                              const Operation& op) const;
  void CallEqualOperatorInt(const std::shared_ptr<VariableInStack>& another,
                            const std::shared_ptr<VariableInStack>& result,
                            const Operation& op) const;
  void CallEqualOperatorString(const std::shared_ptr<VariableInStack>& another,
                               const std::shared_ptr<VariableInStack>& result,
                               const Operation& op) const;
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

  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override;

  Node* children_begin = nullptr;
  Node* children_end = nullptr;
};

struct CodeBlock : public Container {};

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

  Expression(Types type1, Types type2, Operation op);

  void AddStackPointer(stack_pointer d);

  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override;

  static size_t GetPriority(Operation);
  static PriorityType GetPriorityType(Operation);
  static Operation Convert(Lexer::LexerToken::Type);
  Types type1, type2;
  Operation op = Operation::None;
  stack_pointer position_result = 0;
  size_t count;
};

struct BlockFor : public Node {
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override;

  Variable* var = nullptr;
  Expression* check = nullptr;
  Expression* tick = nullptr;
  CodeBlock* code = nullptr;
};

struct BlockIf : public Node {
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override;

  Expression* check = nullptr;
  CodeBlock* code = nullptr;
};

struct BlockWhile : public Node {
  void Run(std::vector<std::shared_ptr<VariableInStack>>& stack) override;

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
  void PushBlockFor(Container* container, const LexerTokenList& list, LexerTokenList::const_iterator& it);
  void PushSignatureBlockFor(BlockFor* node_for, const LexerTokenList& list, LexerTokenList::const_iterator& it);
  void PushBlockIf(Container* container, const LexerTokenList& list, LexerTokenList::const_iterator& it);
  void PushSignatureBlockIf(BlockIf* node_if, const LexerTokenList& list, LexerTokenList::const_iterator& it);
  void PushBlockWhile(Container* container, const LexerTokenList& list, LexerTokenList::const_iterator& it);
  void PushSignatureBlockWhile(BlockWhile* node_if, const LexerTokenList& list, LexerTokenList::const_iterator& it);
  Expression* ParseExpression(LexerTokenList::const_iterator, LexerTokenList::const_iterator);
  Expression* ParseExpression(const LexerTokenList&, LexerTokenList::const_iterator&);
  void PushDeallocateStack(Node*, size_t);
  stack_pointer GetCountStackOffsetForVariable(Node* node, std::string name);
  void LinkVariables(Node*);
  void LinkVariablesInExpression(Expression*, Node*);
  CodeBlock* tree_ = nullptr;
};

