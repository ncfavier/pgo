package main

type A struct { t T }
func main() {}
type B struct { a A }
type T struct { b B }
