#include <iostream>
#include <fstream>
#include "preprocessor.hpp"
#include "lexer.hpp"
#include "syntax_tree.hpp"

std::string program;

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cout << "Invalid file" << std::endl;
    return 1;
  }
  std::ifstream fin(argv[1]);

  {
    std::string str;
    while (getline(fin, str)) {
      str.push_back('\n');
      program += str;
    }
  }

  // std::cout << program << std::endl;

  program = Preprocessor(std::move(program));

  // std::cout << program << std::endl;

  Lexer lexer;
  lexer.ParseText(program);
  auto tokens = lexer.GetTokens();
  /*
  for (auto i: tokens) {
    std::cout << std::to_string(i.type) << " " << i.info << "; ";
  }
  std::cout << std::endl;
  */
  SyntaxTree tree;
  tree.PushLexerTokenList(tokens);
  tree.Compile();
  tree.Run();
  std::cout << "END";
}

