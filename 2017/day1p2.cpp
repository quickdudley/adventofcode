#include <iostream>
#include <fstream>

struct cll {
  char c;
  cll *next;
  ~cll() {
    delete next;
  }
};

int cll_length(cll *node) {
  int r = 0;
  while(node) {
    r++;
    node = node->next;
  }
  return r;
}

cll* cll_rotate(cll *root, int dist) {
  cll *ns = root;
  for(int i = 0; i < dist; i++) {
    ns = ns->next;
  }
  cll *cs = ns;
  cll *ct = NULL;
  cll *nroot = NULL;
  while(cs) {
    cll *n = new cll();
    n->c = cs->c;
    if(ct) {
      ct->next = n;
    } else {
      nroot = n;
    }
    ct = n;
    cs = cs->next;
  }
  cs = root;
  while(cs != ns) {
    cll *n = new cll();
    n->c = cs->c;
    ct->next = n;
    ct = n;
    cs = cs->next;
  }
  ct->next = NULL;
  return nroot;
}

int main(int argc, char** args) {
  std::ifstream input = std::ifstream(args[1]);
  cll *root = NULL;
  cll *current = NULL;
  char c;
  while(input.get(c)) {
    if(c >= '0' && c <= '9') {
      if(current) {
        current->next = new cll();
        current = current->next;
      } else {
        root = new cll();
        current = root;
      }
      current->c = c;
    }
  }
  current->next = NULL;
  cll *root2 = cll_rotate(root, cll_length(root) / 2);
  unsigned long long tally = 0;
  current = root;
  cll *current2 = root2;
  while(current->next) {
    if(current->c == current2->c) {
      tally += current->c - '0';
    }
    current = current->next;
    current2 = current2->next;
  }
  delete root;
  delete root2;
  std::cout << tally << "\n";
}