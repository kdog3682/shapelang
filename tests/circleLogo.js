export {
    shapelangLogo,
}
// import { calculateNumCircles } from "/home/kdog3682/2024-javascript/js-toolkit/mathkit/nerdApps/calculateNumCircles.js"
import { World } from "/home/kdog3682/2024-javascript/shapelang/World.js"

function circleLine({ radius, width, spacing, height = 50 } = {}) {
    // draws a line uniformly dotted with circles

    const world = new World()

    return world.canvas(({ palette, group, circle, rect, line, square, get, register }) => {
        for (let i = 0; i < 10; i++) {
            const p = [10 * i, 0]
            const r = 2
            circle(p, r).fill(palette.black())
        }
    })
}

function shapelangLogo() {
    const world = new World()
    return world.canvas(({palette, origin, group, circle, rect, line}) => {
        function go(r, l, angles) {
            const mapper = (angle) => {
                const p = origin.translate({angle, length: l})
                const c = circle(p, r)
                return c.fill(palette.next())
            }
            const circles = angles.map(mapper)
            world.register(circles)
        }
        const angles = [0, 90, 180, 270]
        let max = 3
        // creating some pretty cool patterns
        for (let i = 0; i < max; i++) {
            let l = (max - i) * (max - i)
            go(max - i, l, angles.map((angl) => angl + 45 * i))
        }
    })
}
// console.log(shapelangLogo())
// console.log(circleLine())
