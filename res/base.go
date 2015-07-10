package main

import (
	"fmt"
)

type CloverInt struct {
	value int
}

type CloverFloat struct {
	value float32
}

type CloverBool struct {
	value bool
}

type CloverString struct {
	value string
}

type CloverObj interface {
	ShowValue() string
}

func (s CloverInt) ShowValue() string {
	return fmt.Sprint(s.value)
}

func (s CloverFloat) ShowValue() string {
	return fmt.Sprint(s.value)
}

func (s CloverBool) ShowValue() string {
	return fmt.Sprint(s.value)
}

func (s CloverString) ShowValue() string {
	return fmt.Sprint(s.value)
}
/*
func main() {
	ci := CloverInt{9}
	c := CloverInt{99}
	op := CloverInt{3}
	bl := CloverBool{true}
	fl := CloverFloat{5.55}
	fmt.Println(ci.ShowValue())
  fmt.Println(Plus(ci, op).ShowValue())
	fmt.Println(Or(bl, CloverBool{false}).ShowValue())
	fmt.Println(Mulf(fl, fl).ShowValue())
	fmt.Println(tes(ci, op).ShowValue())
	println(generated(c, op))
}
*/
func Tes(x, y int) int {
	return x + y
}

// built-in functions
// int
func Plus(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverInt).value
	v2 := o2.(CloverInt).value
	o := CloverInt{v1 + v2}
	return o
}

func Minus(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverInt).value
	v2 := o2.(CloverInt).value
	o := CloverInt{v1 - v2}
	return o
}

func Mul(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverInt).value
	v2 := o2.(CloverInt).value
	o := CloverInt{v1 * v2}
	return o
}

// float
func Plusf(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverFloat).value
	v2 := o2.(CloverFloat).value
	o := CloverFloat{v1 + v2}
	return o
}

func Minusf(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverFloat).value
	v2 := o2.(CloverFloat).value
	o := CloverFloat{v1 - v2}
	return o
}

func Mulf(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverFloat).value
	v2 := o2.(CloverFloat).value
	o := CloverFloat{v1 * v2}
	return o
}

// bool
func And(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverBool).value
	v2 := o2.(CloverBool).value
	o := CloverBool{v1 && v2}
	return o
}

func Or(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverBool).value
	v2 := o2.(CloverBool).value
	o := CloverBool{v1 || v2}
	return o
}

// util
func println(o CloverObj) {
	fmt.Println(o.ShowValue())
}

// user-define

func pluser(x, y CloverObj) CloverObj {
	return Plus(x, y)
}

func tes(x, y interface{}) CloverObj {
	v1 := x.(CloverInt)
	v2 := y.(CloverInt)
	return Plus(v1, v2)
}

// (defn f-name [x y] (+ x y))
// to
// List[Symbol "defn", Symbol "fname", Vector[Symbol "x", Symbol "y"],
//      List[Symbol "+", Symbol "x", Symbol "y"]
// ]
//
