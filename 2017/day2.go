package main

import "fmt"
import "os"
import "bufio"
import "strings"
import "strconv"

func check(e error) {
  if e != nil {
    panic(e)
  }
}

func maxmin(l []int) (int,int) {
  n, x := l[0], l[0]
  for _, v := range l[1:] {
    if(v > x) {
      x = v
    }
    if(v < n) {
      n = v
    }
  }
  return x, n
}

func main() {
  filename := os.Args[1]
  ssFile, error := os.Open(filename)
  check(error)
  ss := bufio.NewScanner(bufio.NewReader(ssFile))
  cs := 0
  for ss.Scan() {
    line := ss.Text()
    ls := bufio.NewScanner(strings.NewReader(line))
    ls.Split(bufio.ScanWords)
    numbers := []int{}
    for ls.Scan() {
      var i int
      i, error = strconv.Atoi(ls.Text())
      numbers = append(numbers, i)
    }
    x, n := maxmin(numbers)
    cs += x - n
  }
  fmt.Println(cs)
}