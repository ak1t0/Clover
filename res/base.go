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

func Not(o CloverObj) CloverObj {
	v := o.(CloverBool).value
	r := CloverBool{!v}
	return r
}

// util
func println(o CloverObj) {
	fmt.Println(o.ShowValue())
}

func Eq(o1, o2 CloverObj) CloverObj {
	switch o1.(type) {
		case CloverInt:
			return CloverBool{(o1.(CloverInt).value == o2.(CloverInt).value)}
		case CloverFloat:
			return CloverBool{(o1.(CloverFloat).value == o2.(CloverFloat).value)}
		case CloverString:
			return CloverBool{(o1.(CloverString).value == o2.(CloverString).value)}
		case CloverBool:
			return CloverBool{(o1.(CloverBool).value == o2.(CloverBool).value)}
	}
	return CloverInt{9999}
}

func Neq(o1, o2 CloverObj) CloverObj {
	return Not(Eq(o1, o2))
}

func tes(x, y interface{}) CloverObj {
	v1 := x.(CloverInt)
	v2 := y.(CloverInt)
	return Plus(v1, v2)
}

/*
func main() {
	fmt.Println(Eq(CloverInt{0}, CloverInt{1}))
	fmt.Println(Eq(CloverBool{}, CloverBool{}))
	fmt.Println(Neq(CloverString{"aaa"}, CloverString{"sss"}))
	fmt.Println(Eq(CloverString{"aaa"}, CloverString{"aaa"}))
}
*/
// (defn f-name [x y] (+ x y))
// to
// List[Symbol "defn", Symbol "fname", Vector[Symbol "x", Symbol "y"],
//      List[Symbol "+", Symbol "x", Symbol "y"]
// ]
//
