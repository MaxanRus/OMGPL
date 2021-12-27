#include <unordered_set>
#include <stdexcept>
#include <limits>
#include <iostream>
#include "syntax_tree.hpp"

using Type = Lexer::LexerToken::Type;

bool IsOperation(Type type) {
  switch (type) {
    case Type::Equal:
    case Type::Plus:
    case Type::Minus:
    case Type::Star:
    case Type::Slash:
    case Type::ExclamationMark:
    case Type::PlusEqual:
    case Type::MinusEqual:
    case Type::StarEqual:
    case Type::SlashEqual:
    case Type::EqualEqual:
    case Type::ExclamationMarkEqual:
    // case Type::PlusPlus:
    // case Type::MinusMinus:
    case Type::LAngle:
    case Type::RAngle:
    case Type::LAngleEqual:
    case Type::RAngleEqual:return true;
    default:return false;
  }
}

void Node::Insert(Node* node) {
  node->previous = this;
  node->parent = parent;
  if (next) {
    next->previous = node;
    node->next = next;
  } else {
    if (parent) {
      dynamic_cast<Container*>(parent)->children_end = node;
    }
  }
  next = node;
}

void Node::InsertBefore(Node* node) {
  node->next = this;
  node->parent = parent;
  if (previous) {
    previous->next = node;
    node->previous = previous;
  } else {
    if (parent) {
      dynamic_cast<Container*>(parent)->children_begin = node;
    }
  }
  previous = node;
}

void Container::AddChildren(Node* node) {
  if (children_end) {
    children_end->next = node;
    node->previous = children_end;
    children_end = node;
    node->parent = this;
  } else {
    children_begin = children_end = node;
    node->parent = this;
  }
}

void Container::Run(std::vector<std::shared_ptr<VariableInStack>>& stack) {
  Node* current = children_begin;
  while (current) {
    current->Run(stack);
    current = current->next;
  }
}

size_t Expression::GetPriority(Operation op) {
  switch (op) {
    case Operation::Equal:
    case Operation::StarEqual:
    case Operation::SlashEqual:
    case Operation::PlusEqual:
    case Operation::MinusEqual:
      return 0;
    case Operation::EqualEqual:
    case Operation::ExclamationMarkEqual:
      return 6;
    case Operation::LAngle:
    case Operation::LAngleEqual:
    case Operation::RAngle:
    case Operation::RAngleEqual:
      return 7;
    case Operation::Plus:
    case Operation::Minus:
      return 9;
    case Operation::Star:
    case Operation::Slash:
      return 10;
  }
  throw std::logic_error("unsupported operator");
}

Operation Expression::Convert(Lexer::LexerToken::Type type) {
  switch (type) {
    case Type::Equal:return Operation::Equal;
    case Type::LAngle:return Operation::LAngle;
    case Type::Plus:return Operation::Plus;
    case Type::Minus:return Operation::Minus;
    case Type::Star:return Operation::Star;
    case Type::Slash:return Operation::Slash;
    case Type::ExclamationMark:return Operation::ExclamationMark;
    case Type::PlusEqual:return Operation::PlusEqual;
    case Type::MinusEqual:return Operation::MinusEqual;
    case Type::StarEqual:return Operation::StarEqual;
    case Type::SlashEqual:return Operation::SlashEqual;
    case Type::EqualEqual:return Operation::EqualEqual;
    case Type::ExclamationMarkEqual:return Operation::ExclamationMarkEqual;
      // case Type::PlusPlus:
      // case Type::MinusMinus:
    case Type::RAngle:return Operation::RAngle;
    case Type::LAngleEqual:return Operation::LAngleEqual;
    case Type::RAngleEqual:return Operation::RAngleEqual;
  }
  std::cerr << std::to_string(type) << std::endl;
  throw std::logic_error("unsupported operator");
}

PriorityType Expression::GetPriorityType(Operation op) {
  switch (op) {
    case Operation::Equal:
    case Operation::StarEqual:
    case Operation::SlashEqual:
    case Operation::PlusEqual:
    case Operation::MinusEqual:
      return PriorityType::R;
    case Operation::EqualEqual:
    case Operation::ExclamationMarkEqual:
    case Operation::LAngle:
    case Operation::LAngleEqual:
    case Operation::RAngle:
    case Operation::RAngleEqual:
    case Operation::Plus:
    case Operation::Minus:
    case Operation::Star:
    case Operation::Slash:
      return PriorityType::L;
  }

  throw std::logic_error("unsupported operator");
}

void Expression::Run(std::vector<std::shared_ptr<VariableInStack>>& stack) {
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

void Expression::AddStackPointer(stack_pointer d) {
  position_result += d;
}

Expression::Expression(Expression::Types type1, Expression::Types type2, Operation op) : type1(std::move(type1)), type2(std::move(type2)), op(op) {
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

bool SyntaxTree::IsTypeName(Node* node, const std::string& str) {
  return str == "int" || str == "string";
}

bool SyntaxTree::IsVariableName(Node* node, const std::string& name) {
  while (node) {
    if (auto var = dynamic_cast<Variable*>(node)) {
      if (var->name == name) {
        return true;
      }
    }
    if (node->previous) {
      node = node->previous;
    } else {
      node = node->parent;
    }
  }
  return false;
}

bool SyntaxTree::IsFunctionName(Node*, const std::string& str) {
  return str == "print";
}

void SyntaxTree::PushLexerTokenList(const LexerTokenList& list) {
  if (!tree_) {
    tree_ = new CodeBlock();
  }
  for (auto it = list.begin(); it != list.end(); ++it) {
    PushLine(tree_, list, it);
  }
}

CodeBlock* SyntaxTree::ParseCurlyBrackets(Container* container,
                                          const LexerTokenList& list,
                                          LexerTokenList::const_iterator& it) {
  auto* current = new CodeBlock();
  current->previous = container->children_end;
  current->parent = container;

  for (; it->type != Type::CurlyCloseBracket; ++it) {
    PushLine(current, list, it);
  }
  return current;
}

void SyntaxTree::PushCurlyBrackets(Container* container, const LexerTokenList& list,
                                   LexerTokenList::const_iterator& it) {
  container->AddChildren(ParseCurlyBrackets(container, list, it));
}

void SyntaxTree::PushLine(Container* container, const LexerTokenList& list,
                          LexerTokenList::const_iterator& it) {
  if (it->type == Type::CurlyOpenBracket) {
    PushCurlyBrackets(container, list, ++it);
  } else if (it->type == Type::RoundOpenBracket) {
    PushExpression(container, list, it);
  } else if (it->type == Type::IF) {
    PushBlockIf(container, list, it);
  } else if (it->type == Type::FOR) {
    PushBlockFor(container, list, it);
  } else if (it->type == Type::WHILE) {
    PushBlockWhile(container, list, it);
  } else if (it->type == Type::Word) {
    auto place = container->children_end;
    if (!place)
      place = container;
    if (it->info == "def") {
      // PushFunction(container, list, ++it);
    } else if (IsTypeName(place, it->info)) {
      PushNewVariable(container, list, it);
    } else if (IsVariableName(place, it->info)) {
      PushExpression(container, list, it);
    } else if (IsFunctionName(place, it->info)) {
      PushCallFunction(container, list, it);
    }
  }
}

Variable* SyntaxTree::ParseNewVariable(const LexerTokenList& list, LexerTokenList::const_iterator& it) {
  std::string type = it->info;
  ++it;
  if (it->type == Type::Word) {
    auto var = new Variable(std::move(type), it->info);
    ++it;
    if (it->type == Type::Semicolon)
      return var;
    if (it->type == Type::Equal) {
      --it;
      var->default_value = ParseExpression(list, it);
      if (it->type == Type::Semicolon) {
        return var;
      } else {
        throw std::logic_error("Expected ;");
      }
    } else {
      throw std::logic_error("Expected =");
    }
  } else {
    throw std::logic_error("Expected variable name");
  }
}

void SyntaxTree::PushNewVariable(Container* container, const LexerTokenList& list, LexerTokenList::const_iterator& it) {
  container->AddChildren(ParseNewVariable(list, it));
}

void SyntaxTree::PushExpression(Container* container, const LexerTokenList& list, LexerTokenList::const_iterator& it) {
  container->AddChildren(ParseExpression(list, it));
}

void SyntaxTree::PushParametersFunction(CallFunction* call_function,
                                        const LexerTokenList& list,
                                        LexerTokenList::const_iterator& it) {
  if (it->type != Type::RoundOpenBracket) {
    throw std::logic_error("expected ( after function name");
  }
  while (it->type != Type::RoundCloseBracket) {
    auto expr = ParseExpression(list, ++it);
    call_function->parameters.push_back(new Expression(new Expression((size_t) 0, nullptr, Operation::Value),
                                                       expr,
                                                       Operation::Equal));
    auto& t = call_function->parameters.back();
    t->parent = call_function;
    std::get<Expression*>(t->type1)->parent = t;
    std::get<Expression*>(t->type2)->parent = t;
  }
  for (size_t i = 0; i < call_function->parameters.size(); ++i) {
    std::get<Expression*>(call_function->parameters[i]->type1)->type1 = call_function->parameters.size() - i;
  }
  ++it;
}

void SyntaxTree::PushCallFunction(Container* container,
                                  const LexerTokenList& list,
                                  LexerTokenList::const_iterator& it) {
  auto function = new CallFunction(it->info);
  PushParametersFunction(function, list, ++it);
  container->AddChildren(new CreateVariables(function->parameters.size()));
  container->AddChildren(function);
  container->AddChildren(new HiddenDeallocateStack(function->parameters.size()));
}

void SyntaxTree::PushBlockFor(Container* container,
                              const LexerTokenList& list,
                              std::list<Lexer::LexerToken>::const_iterator& it) {
  auto c = new Container;
  auto f = new BlockFor();
  container->AddChildren(c);
  PushSignatureBlockFor(f, list, ++it);
  c->AddChildren(f->var);
  c->AddChildren(f);
  if (it->type != Type::CurlyOpenBracket) {
    throw std::logic_error("expected { after for (...)");
  }
  f->code = ParseCurlyBrackets(c, list, ++it);
  f->code->parent = f;
  f->code->previous = nullptr;
}

void SyntaxTree::PushSignatureBlockFor(BlockFor* node_for,
                                       const LexerTokenList& list,
                                       std::list<Lexer::LexerToken>::const_iterator& it) {
  if (it->type != Type::RoundOpenBracket)
    throw std::logic_error("expected ( after for");
  ++it;
  if (it->type == Type::Semicolon) {
    ++it;
  } else {
    if (it->type != Type::Word || !IsTypeName(node_for, it->info)) {
      throw std::logic_error("expected type after for (");
    }
    node_for->var = ParseNewVariable(list, it);
    node_for->var->parent = node_for;

    if (it->type != Type::Semicolon) {
      throw std::logic_error("expected ; after for ( type name");
    }
    ++it;
  }
  if (it->type == Type::Semicolon) {
    ++it;
  } else {
    node_for->check = ParseExpression(list, it);
    node_for->check->parent = node_for;
    if (it->type != Type::Semicolon) {
      throw std::logic_error("expected ; after for ( type name; expr");
    }
    ++it;
  }
  if (it->type == Type::RoundCloseBracket) {
  } else {
    node_for->tick = ParseExpression(list, it);
    node_for->tick->parent = node_for;
    if (it->type != Type::RoundCloseBracket) {
      throw std::logic_error("expected ) after for ( type name; expr; expr");
    }
    ++it;
  }
}

void SyntaxTree::PushBlockIf(Container* container,
                             const LexerTokenList& list,
                             std::list<Lexer::LexerToken>::const_iterator& it) {
  auto f = new BlockIf();
  container->AddChildren(f);
  PushSignatureBlockIf(f, list, ++it);
  f->check->parent = f;
  if (it->type != Type::CurlyOpenBracket) {
    throw std::logic_error("expected { after if (...)");
  }
  f->code = ParseCurlyBrackets(container, list, ++it);
  f->code->parent = f;
}

void SyntaxTree::PushSignatureBlockIf(BlockIf* node_if,
                                      const LexerTokenList& list,
                                      std::list<Lexer::LexerToken>::const_iterator& it) {
  if (it->type != Type::RoundOpenBracket)
    throw std::logic_error("expected ( after if");
  ++it;
  node_if->check = ParseExpression(list, it);
  node_if->check->parent = node_if;
  if (it->type != Type::RoundCloseBracket) {
    throw std::logic_error("expected ) after if(...");
  }
  ++it;
}

void SyntaxTree::PushBlockWhile(Container* container,
                                const LexerTokenList& list,
                                std::list<Lexer::LexerToken>::const_iterator& it) {
  auto f = new BlockWhile();
  container->AddChildren(f);
  PushSignatureBlockWhile(f, list, ++it);
  if (it->type != Type::CurlyOpenBracket) {
    throw std::logic_error("expected { after while (...)");
  }
  f->code = ParseCurlyBrackets(container, list, ++it);
}

void SyntaxTree::PushSignatureBlockWhile(BlockWhile* node_if,
                                         const LexerTokenList& list,
                                         std::list<Lexer::LexerToken>::const_iterator& it) {
  if (it->type != Type::RoundOpenBracket)
    throw std::logic_error("expected ( after while");
  ++it;
  node_if->check = ParseExpression(list, it);
  node_if->check->parent = node_if;
  if (it->type != Type::RoundCloseBracket) {
    throw std::logic_error("expected ) after while(...");
  }
  ++it;
}

Expression* SyntaxTree::ParseExpression(LexerTokenList::const_iterator l, LexerTokenList::const_iterator r) {
  if (std::next(l) == r) {
    return new Expression(l->info, nullptr, Operation::Value);
  }
  {
    auto it = l;
    if (it->type == Type::RoundOpenBracket) {
      int balance = 1;
      do {
        ++it;
        if (it->type == Type::RoundOpenBracket) {
          ++balance;
        } else if (it->type == Type::RoundCloseBracket) {
          --balance;
        }
      } while (balance != 0);
    }
    ++it;
    if (it == r) {
      return ParseExpression(std::next(l), std::prev(r));
    }
  }
  auto mid = l;
  size_t current_priority = std::numeric_limits<size_t>::max();
  for (auto it = l; std::next(it) != r; ++it) {
    if (it->type == Type::RoundOpenBracket) {
      int balance = 1;
      do {
        ++it;
        if (it->type == Type::RoundOpenBracket) {
          ++balance;
        } else if (it->type == Type::RoundCloseBracket) {
          --balance;
        }
      } while (balance != 0);
    }
    ++it;

    size_t tmp = Expression::GetPriority(Expression::Convert(it->type));
    if (tmp < current_priority) {
      mid = it;
      current_priority = tmp;
    } else if (tmp == current_priority) {
      if (Expression::GetPriorityType(Expression::Convert(it->type)) == PriorityType::R) {
        mid = it;
      }
    }
  }
  return new Expression(ParseExpression(l, mid), ParseExpression(std::next(mid), r), Expression::Convert(mid->type));
}

Expression* SyntaxTree::ParseExpression(const LexerTokenList& list,
                                        std::list<Lexer::LexerToken>::const_iterator& it) {
  auto l = it;
  int balance = 0;
  while (it->type == Type::Word || IsOperation(it->type) || it->type == Type::StringLiteral
      || it->type == Type::RoundOpenBracket || it->type == Type::RoundCloseBracket) {
    if (it->type == Type::RoundOpenBracket) {
      ++balance;
    } else if (it->type == Type::RoundCloseBracket) {
      if (balance == 0) {
        break;
      } else {
        --balance;
      }
    }
    ++it;
  }
  return ParseExpression(l, it);
}

void SyntaxTree::PushDeallocateStack(Node* node, size_t count_variables = 0) {
  if (auto code_block = dynamic_cast<Container*>(node)) {
    if (code_block->children_begin) {
      PushDeallocateStack(code_block->children_begin);
    }
  }
  if (auto if_ = dynamic_cast<BlockIf*>(node)) {
    PushDeallocateStack(if_->code);
  }
  if (auto for_ = dynamic_cast<BlockFor*>(node)) {
    PushDeallocateStack(for_->code);
  }
  if (dynamic_cast<Variable*>(node)) {
    ++count_variables;
  }
  if (auto expr = dynamic_cast<Expression*>(node)) {
    node->Insert(new DeallocateStack(1));
  }
  if (node->next) {
    PushDeallocateStack(node->next, count_variables);
  } else {
    if (count_variables) {
      node->Insert(new DeallocateStack(count_variables));
    }
  }
}

void SyntaxTree::Compile() {
  PushDeallocateStack(tree_);
  LinkVariables(tree_);
}

void SyntaxTree::Run() {
  std::vector<std::shared_ptr<VariableInStack>> stack;
  tree_->Run(stack);
}

void SyntaxTree::LinkVariables(Node* node) {
  while (node) {
    if (auto expr = dynamic_cast<Expression*>(node)) {
      LinkVariablesInExpression(expr, node);
    }
    if (auto var = dynamic_cast<Variable*>(node)) {
      if (var->default_value) {
        LinkVariablesInExpression(var->default_value, node);
      }
    }
    if (auto if_ = dynamic_cast<BlockIf*>(node)) {
      LinkVariablesInExpression(if_->check, node);
      LinkVariables(if_->code->children_begin);
    }
    if (auto while_ = dynamic_cast<BlockWhile*>(node)) {
      LinkVariablesInExpression(while_->check, node);
      LinkVariables(while_->code->children_begin);
    }
    if (auto for_ = dynamic_cast<BlockFor*>(node)) {
      LinkVariablesInExpression(for_->check, node);
      LinkVariablesInExpression(for_->tick, node);
      LinkVariables(for_->code->children_begin);
    }
    if (auto call_function = dynamic_cast<CallFunction*>(node)) {
      for (auto i: call_function->parameters) {
        LinkVariablesInExpression(i, node);
      }
    }
    if (auto container = dynamic_cast<Container*>(node)) {
      LinkVariables(container->children_begin);
    }
    node = node->next;
  }
}

stack_pointer SyntaxTree::GetCountStackOffsetForVariable(Node* node, std::string name) {
  stack_pointer result = 0;
  while (node) {
    if (auto var = dynamic_cast<Variable*>(node)) {
      result += 1;
      if (var->name == name) {
        return result;
      }
    }
    if (auto vars = dynamic_cast<CreateVariables*>(node)) {
      result += vars->count;
    }
    if (auto vars = dynamic_cast<HiddenDeallocateStack*>(node)) {
      result -= vars->count;
    }
    if (node->previous) {
      node = node->previous;
    } else {
      node = node->parent;
    }
  }
  return std::numeric_limits<stack_pointer>::max();
}

void SyntaxTree::LinkVariablesInExpression(Expression* expression, Node* node) {
  if (expression->op == Operation::Value) {
    if (std::holds_alternative<std::string>(expression->type1)) {
      auto& var = std::get<std::string>(expression->type1);
      stack_pointer offset = GetCountStackOffsetForVariable(node, var);
      if (offset != std::numeric_limits<stack_pointer>::max()) {
        expression->type1 = offset;
      }
    }
  } else {
    LinkVariablesInExpression(std::get<Expression*>(expression->type1), node);
    LinkVariablesInExpression(std::get<Expression*>(expression->type2), node);
  }
}

void CallFunction::Run(std::vector<std::shared_ptr<VariableInStack>>& stack) {
  for (size_t i = 0; i < parameters.size(); ++i) {
    stack.push_back(std::make_shared<VariableInStack>(TypeVariable()));
    // stack.back().Allocate();
  }
  for (auto i: parameters) {
    i->Run(stack);
    stack.pop_back();
  }
  if (name_function == "print") {
    for (size_t i = 0; i < parameters.size(); ++i) {
      std::shared_ptr<VariableInStack>& t = *(stack.end() - parameters.size() + i);
      if (t->type_variable.id == TypeVariable::ID::type_int) {
        std::cout << *((int*) t->memory) << " ";
      } else if (t->type_variable.id == TypeVariable::ID::type_string) {
        std::cout << *((std::string*) t->memory) << " ";
      }
    }
    std::cout << std::endl;
  }
}

void Variable::Run(std::vector<std::shared_ptr<VariableInStack>>& stack) {
  if (type == "int") {
    stack.push_back(std::make_shared<VariableInStack>(TypeVariable(TypeVariable::ID::type_int)));
  } else if (type == "string") {
    stack.push_back(std::make_shared<VariableInStack>(TypeVariable(TypeVariable::ID::type_string)));
  }
  stack.back()->Allocate();
  if (default_value) {
    default_value->Run(stack);
    stack.pop_back();
  }
}

void VariableInStack::CallOperator(std::shared_ptr<VariableInStack>& another,
                                   std::shared_ptr<VariableInStack>& result,
                                   Operation op) {
  if (op == Operation::Equal || op == Operation::PlusEqual || op == Operation::MinusEqual
      || op == Operation::StarEqual || op == Operation::SlashEqual) {
    CallEqualOperator(another, result, op);
    return;
  }
  if (op == Operation::EqualEqual || op == Operation::ExclamationMarkEqual || op == Operation::LAngle
      || op == Operation::RAngle || op == Operation::LAngleEqual || op == Operation::RAngleEqual) {
    CallComparisonOperator(another, result, op);
    return;
  }
  if (op == Operation::Plus || op == Operation::Minus || op == Operation::Star || op == Operation::Slash) {
    CallArithmeticOperator(another, result, op);
    return;
  }
}

void VariableInStack::CallArithmeticOperator(const std::shared_ptr<VariableInStack>& another,
                                             std::shared_ptr<VariableInStack>& result,
                                             const Operation& op) const {
  result->Clear();
  result->type_variable.id = another->type_variable.id;
  result->Allocate();
  if (another->type_variable.id == TypeVariable::type_int && type_variable.id == TypeVariable::type_int) {
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
  } else if (another->type_variable.id == TypeVariable::type_string
      && type_variable.id == TypeVariable::type_string) {
    std::string& x = *static_cast<std::string*>(memory);
    std::string& y = *static_cast<std::string*>(another->memory);
    std::string& res = *static_cast<std::string*>(result->memory);
    if (op == Operation::Plus) {
      res = (x + y);
    }
  }
}

void VariableInStack::CallComparisonOperator(const std::shared_ptr<VariableInStack>& another,
                                             std::shared_ptr<VariableInStack>& result,
                                             const Operation& op) const {
  result->Clear();
  result->type_variable.id = TypeVariable::type_int;
  result->Allocate();
  int& res = *static_cast<int*>(result->memory);

  if (another->type_variable.id == TypeVariable::type_int && type_variable.id == TypeVariable::type_int) {
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
  } else if (another->type_variable.id == TypeVariable::type_string
      && type_variable.id == TypeVariable::type_string) {
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
}

void VariableInStack::CallEqualOperator(const std::shared_ptr<VariableInStack>& another,
                                        std::shared_ptr<VariableInStack>& result,
                                        const Operation& op) {
  if (type_variable.id != TypeVariable::none && type_variable.id != another->type_variable.id) {
    throw std::logic_error("error variable convert");
  }
  if (result->type_variable.id != TypeVariable::none && result->type_variable.id != another->type_variable.id) {
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
  if (another->type_variable.id == TypeVariable::type_int) {
    CallEqualOperatorInt(another, result, op);
  } else if (another->type_variable.id == TypeVariable::type_string) {
    CallEqualOperatorString(another, result, op);
  }
}

void VariableInStack::CallEqualOperatorString(const std::shared_ptr<VariableInStack>& another,
                                              const std::shared_ptr<VariableInStack>& result,
                                              const Operation& op) const {
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

void VariableInStack::CallEqualOperatorInt(const std::shared_ptr<VariableInStack>& another,
                                           const std::shared_ptr<VariableInStack>& result,
                                           const Operation& op) const {
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
}

void VariableInStack::Clear() {
  if (memory) {
    if (type_variable.id == TypeVariable::ID::type_int) {
      delete static_cast<int*>(memory);
    } else if (type_variable.id == TypeVariable::ID::type_string) {
      delete static_cast<std::string*>(memory);
    }
  }
  memory = nullptr;
}

void VariableInStack::Allocate() {
  if (type_variable.id == TypeVariable::ID::type_int) {
    memory = new int();
  } else if (type_variable.id == TypeVariable::ID::type_string) {
    memory = new std::string();
  }
}

void BlockWhile::Run(std::vector<std::shared_ptr<VariableInStack>>& stack) {
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

void BlockIf::Run(std::vector<std::shared_ptr<VariableInStack>>& stack) {
  if (check) {
    check->Run(stack);
    auto res = *((int*) stack.back()->memory);
    stack.pop_back();
    if (res) {
      code->Run(stack);
    }
  }
}

void BlockFor::Run(std::vector<std::shared_ptr<VariableInStack>>& stack) {
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

void HiddenDeallocateStack::Run(std::vector<std::shared_ptr<VariableInStack>>& stack) {
  for (size_t i = 0; i < count; ++i) {
    stack.pop_back();
  }
}

void DeallocateStack::Run(std::vector<std::shared_ptr<VariableInStack>>& stack) {
  for (size_t i = 0; i < count; ++i) {
    stack.pop_back();
  }
}
