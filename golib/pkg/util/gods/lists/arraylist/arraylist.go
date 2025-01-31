// Copyright (c) 2015, Emir Pasic. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package arraylist implements the array list.
//
// Structure is not thread safe.
//
// Reference: https://en.wikipedia.org/wiki/List_%28abstract_data_type%29
package arraylist

import (
	"fmt"
	"strings"

	"github.com/emirpasic/gods/lists"
	"github.com/emirpasic/gods/utils"
)

// Assert List implementation
var _ lists.List = (*List)(nil)

// List holds the Elements in a slice
type List struct {
	Elements []interface{}
	SizeNum  int
}

const (
	growthFactor = float32(2.0)  // growth by 100%
	shrinkFactor = float32(0.25) // shrink when SizeNum is 25% of capacity (0 means never shrink)
)

// New instantiates a new list and adds the passed values, if any, to the list
func New(values ...interface{}) *List {
	list := &List{}
	if len(values) > 0 {
		list.Add(values...)
	}
	return list
}

// Add appends a value at the end of the list
func (list *List) Add(values ...interface{}) {
	list.growBy(len(values))
	for _, value := range values {
		list.Elements[list.SizeNum] = value
		list.SizeNum++
	}
}

// Get returns the element at index.
// Second return parameter is true if index is within bounds of the array and array is not empty, otherwise false.
func (list *List) Get(index int) (interface{}, bool) {

	if !list.withinRange(index) {
		return nil, false
	}

	return list.Elements[index], true
}

// Remove removes the element at the given index from the list.
func (list *List) Remove(index int) {

	if !list.withinRange(index) {
		return
	}

	list.Elements[index] = nil                                       // cleanup reference
	copy(list.Elements[index:], list.Elements[index+1:list.SizeNum]) // shift to the left by one (slow operation, need ways to optimize this)
	list.SizeNum--

	list.shrink()
}

// Contains checks if Elements (one or more) are present in the set.
// All Elements have to be present in the set for the method to return true.
// Performance time complexity of n^2.
// Returns true if no arguments are passed at all, i.e. set is always super-set of empty set.
func (list *List) Contains(values ...interface{}) bool {

	for _, searchValue := range values {
		found := false
		for index := 0; index < list.SizeNum; index++ {
			if list.Elements[index] == searchValue {
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	return true
}

// Values returns all Elements in the list.
func (list *List) Values() []interface{} {
	newElements := make([]interface{}, list.SizeNum, list.SizeNum)
	copy(newElements, list.Elements[:list.SizeNum])
	return newElements
}

// IndexOf returns index of provided element
func (list *List) IndexOf(value interface{}) int {
	if list.SizeNum == 0 {
		return -1
	}
	for index, element := range list.Elements {
		if element == value {
			return index
		}
	}
	return -1
}

// Empty returns true if list does not contain any Elements.
func (list *List) Empty() bool {
	return list.SizeNum == 0
}

// Size returns number of Elements within the list.
func (list *List) Size() int {
	return list.SizeNum
}

// Clear removes all Elements from the list.
func (list *List) Clear() {
	list.SizeNum = 0
	list.Elements = []interface{}{}
}

// Sort sorts values (in-place) using.
func (list *List) Sort(comparator utils.Comparator) {
	if len(list.Elements) < 2 {
		return
	}
	utils.Sort(list.Elements[:list.SizeNum], comparator)
}

// Swap swaps the two values at the specified positions.
func (list *List) Swap(i, j int) {
	if list.withinRange(i) && list.withinRange(j) {
		list.Elements[i], list.Elements[j] = list.Elements[j], list.Elements[i]
	}
}

// Insert inserts values at specified index position shifting the value at that position (if any) and any subsequent Elements to the right.
// Does not do anything if position is negative or bigger than list's SizeNum
// Note: position equal to list's SizeNum is valid, i.e. append.
func (list *List) Insert(index int, values ...interface{}) {

	if !list.withinRange(index) {
		// Append
		if index == list.SizeNum {
			list.Add(values...)
		}
		return
	}

	l := len(values)
	list.growBy(l)
	list.SizeNum += l
	copy(list.Elements[index+l:], list.Elements[index:list.SizeNum-l])
	copy(list.Elements[index:], values)
}

// Set the value at specified index
// Does not do anything if position is negative or bigger than list's SizeNum
// Note: position equal to list's SizeNum is valid, i.e. append.
func (list *List) Set(index int, value interface{}) {

	if !list.withinRange(index) {
		// Append
		if index == list.SizeNum {
			list.Add(value)
		}
		return
	}

	list.Elements[index] = value
}

// String returns a string representation of container
func (list *List) String() string {
	str := "ArrayList\n"
	values := []string{}
	for _, value := range list.Elements[:list.SizeNum] {
		values = append(values, fmt.Sprintf("%v", value))
	}
	str += strings.Join(values, ", ")
	return str
}

// Check that the index is within bounds of the list
func (list *List) withinRange(index int) bool {
	return index >= 0 && index < list.SizeNum
}

func (list *List) resize(cap int) {
	newElements := make([]interface{}, cap, cap)
	copy(newElements, list.Elements)
	list.Elements = newElements
}

// Expand the array if necessary, i.e. capacity will be reached if we add n elements
func (list *List) growBy(n int) {
	// When capacity is reached, grow by a factor of growthFactor and add number of Elements
	currentCapacity := cap(list.Elements)
	if list.SizeNum+n >= currentCapacity {
		newCapacity := int(growthFactor * float32(currentCapacity+n))
		list.resize(newCapacity)
	}
}

// Shrink the array if necessary, i.e. when size is shrinkFactor percent of current capacity
func (list *List) shrink() {
	if shrinkFactor == 0.0 {
		return
	}
	// Shrink when SizeNum is at shrinkFactor * capacity
	currentCapacity := cap(list.Elements)
	if list.SizeNum <= int(float32(currentCapacity)*shrinkFactor) {
		list.resize(list.SizeNum)
	}
}
