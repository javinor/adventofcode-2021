const fs = require('fs')
const _ = require('lodash')

const DAY = "01"
const example = fs.readFileSync(`../input/day${DAY}.example`, 'utf8')
const real = fs.readFileSync(`../input/day${DAY}.real`, 'utf8')

const parseInput = input => {
  const lines = input.split('\n')
  return lines.map(x => parseInt(x, 10))
}

const countIncreases = (numbers) => {
  const prev = numbers.slice(0, -1)
  const curr = numbers.slice(1)
  const increases = _.zip(prev, curr).map(([p, c]) => ((c - p) > 0) ? 1 : 0)
  return _.sum(increases)
}

const q1 = countIncreases

const sumSlidingWindow = (numbers) => {
  const xs = numbers.slice(0, -2)
  const ys = numbers.slice(1, -1)
  const zs = numbers.slice(2)

  return _.zip(xs, ys, zs).map(_.sum)
}

const q2 = (numbers) => {
  return countIncreases(sumSlidingWindow(numbers))
}

console.log("q1 example: ", q1(parseInput(example)))
console.log("q1 input: ", q1(parseInput(real)))

console.log("q2 example: ", q2(parseInput(example)))
console.log("q2 input: ", q2(parseInput(real)))