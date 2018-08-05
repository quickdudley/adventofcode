#include <iostream>
#include <fstream>

struct cll {
  char c;
  cll *next;
};

int main(int argc, char** args) {
  std::ifstream input = std::ifstream(args[1]);
  char c0;
  input.get(c0);
  cll *root = new cll();
  root->c = c0;
  cll *current = root;
  while(!input.eof()) {
    current->next = new cll();
    current = current->next;
    input.get(current->c);
  }
  current->c = c0;
  current->next = NULL;
  unsigned long long tally = 0;
  current = root;
  while(current->next) {
    if(current->c == current->next->c) {
      tally += current->c - '0';
    }
    current = current->next;
  }
  current = root;
  while(current) {
    cll* t = current->next;
    delete current;
    current = t;
  }
  std::cout << tally << "\n";
}