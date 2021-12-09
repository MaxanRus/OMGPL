#include "preprocessor.hpp"
#include <vector>

#include <iostream> // TODO

std::string DeleteComments(std::string&& text) {
  std::vector<std::pair<int, int>> text_without_comments;
  size_t n = text.size();
  int begin_segment = 0;

  text_without_comments.emplace_back(begin_segment, n);

  for (size_t i = 0; i < n; ++i) {
    if (text[i] == '/' && i + 1 < n && text[i + 1] == '/') {
      text_without_comments.pop_back();
      text_without_comments.emplace_back(begin_segment, i);
      while (i + 1 < n && text[i + 1] != '\n') {
        ++i;
      }
      begin_segment = i + 1;
      text_without_comments.emplace_back(begin_segment, n);
    }
  }

  int current_place = 0;
  for (auto i: text_without_comments) {
    std::copy(text.begin() + i.first, text.begin() + i.second, text.begin() +
              current_place);
    current_place += i.second - i.first;
  }
  text.resize(current_place);
  return std::move(text);
}

std::string DeleteMultiLineComments(std::string&& text) {
  std::vector<std::pair<int, int>> text_without_comments;
  size_t n = text.size();
  int begin_segment = 0;

  text_without_comments.emplace_back(begin_segment, n);

  for (size_t i = 0; i < n; ++i) {
    if (text[i] == '/' && i + 1 < n && text[i + 1] == '*') {
      text_without_comments.pop_back();
      text_without_comments.emplace_back(begin_segment, i);
      i++;
      while (i < n && (text[i - 1] != '*' || text[i] != '/')) {
        ++i;
      }
      begin_segment = i + 1;
      text_without_comments.emplace_back(begin_segment, n);
    }
  }

  int current_place = 0;
  for (auto i: text_without_comments) {
    std::copy(text.begin() + i.first, text.begin() + i.second, text.begin() +
              current_place);
    current_place += i.second - i.first;
  }
  text.resize(current_place);
  return std::move(text);
}

std::string Preprocessor(std::string&& text) {
  text = DeleteComments(std::move(text));
  text = DeleteMultiLineComments(std::move(text));
  return std::move(text);
}

std::string Preprocessor(const std::string& text) {
  std::string copy(text);
  copy = Preprocessor(std::move(copy));
  return copy;
}
