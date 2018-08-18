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

func evdiv(l []int) int {
  for i, a := range l[0:len(l) - 1] {
    for _, b := range l[i + 1 : len(l)] {
      if a % b == 0 {
        return a / b
      } else if b % a == 0 {
        return b / a
      }
    }
  }
  return -1
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
    cs += evdiv(numbers)
  }
  fmt.Println(cs)
}