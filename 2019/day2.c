#include <stdlib.h>
#include <stdio.h>

typedef struct ll {
  int val;
  struct ll *nxt;
} ll;

int step(int *prog, int *pc);

int* do_read(int *len);
int ll_length(ll *head);
ll* ll_read(void);
void ll_free(ll *head);
int* to_a(ll *head, int *len);

void main(void) {
  int proglen = 0;
  int pc = 0;
  int *prog = NULL;
  prog = do_read(&proglen);
  prog[1] = 12;
  prog[2] = 2;
  while(step(prog, &pc)) {}
  printf("%d\n",prog[0]);
}

int step(int *prog, int *pc) {
  switch(prog[*pc]) {
  case 1:
    prog[prog[(*pc) + 3]] = prog[prog[(*pc) + 1]] + prog[prog[(*pc) + 2]];
    *pc += 4;
    return 1;
  case 2:
    prog[prog[(*pc) + 3]] = prog[prog[(*pc) + 1]] * prog[prog[(*pc) + 2]];
    *pc += 4;
    return 1;
  default:
    return 0;
  }
}

int* do_read(int *len) {
  ll* as_list = NULL;
  int *r = NULL;
  as_list = ll_read();
  r = to_a(as_list, len);
  ll_free(as_list);
  return r;
}

int* to_a(ll *head, int *len) {
  int *r = NULL;
  int i = 0;
  if(!head)
    return NULL;
  *len = ll_length(head);
  r = malloc(*len * sizeof(int));
  while(head) {
    r[i++] = head->val;
    head = head->nxt;
  }
  return r;
}

ll* ll_read(void) {
  ll *head = NULL;
  ll *cur = NULL;
  ll *tmp = NULL;
  int x;
  while(!feof(stdin)) {
    scanf("%d",&x);
    scanf(",");
    tmp = malloc(sizeof(ll));
    tmp->val = x;
        if(!head) {
          head = tmp;
        }
    if(cur) {
      cur->nxt = tmp;
    }
    cur = tmp;
  }
  return head;
}

int ll_length(ll *head) {
  int r = 0;
  while(head) {
    r++;
    head = head->nxt;
  }
  return r;
}

void ll_free(ll *head) {
  ll *nxt;
  while(head) {
    nxt = head->nxt;
    free(head);
    head = nxt;
  }
}
