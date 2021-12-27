#include <gtest/gtest.h>
#include <iostream>
#include <fstream>
#include "preprocessor.hpp"
#include "lexer.hpp"
#include "syntax_tree.hpp"

std::string LoadFile(const std::string& file) {
  std::string program;
  std::ifstream fin(file);
  {
    std::string str;
    while (getline(fin, str)) {
      str.push_back('\n');
      program += str;
    }
  }
  return program;
}

void RunTest(const std::string& program_file, const std::string& result_file) {
  std::string program = LoadFile(program_file);
  program = Preprocessor(std::move(program));
  Lexer lexer;
  lexer.ParseText(program);
  auto tokens = lexer.GetTokens();
  SyntaxTree tree;
  tree.PushLexerTokenList(tokens);
  tree.Compile();

  SetPrintStringStream();
  tree.Run();
  std::string result = LoadFile(result_file);
  ASSERT_EQ(result, GetPrintStringStream().str());
}

TEST(test_int, full_test) {
  RunTest("programs/int/1.omgpl", "programs/int/1.res");
}

TEST(test_string, full_test) {
  RunTest("programs/string/1.omgpl", "programs/string/1.res");
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}

