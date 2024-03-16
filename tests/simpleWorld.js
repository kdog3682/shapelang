import * as variables from "/home/kdog3682/2023/variables.js"
import {World} from "/home/kdog3682/2024-javascript/shapelang/World.js"



// expect to see 3 rectangles
// pretty cool stuff

const world = new World()
const colors = variables.roygbiv
world.canvas(({rect, line, square, get, register}) => {
    rect('p00', 'p23')
    for (let i = 1; i < 3; i++) {
        register(get('a').translate(i, i).fill(colors[i]))
    }
    // console.log(get('rects').length) // 3
})

console.loggg(world.toJSON())
