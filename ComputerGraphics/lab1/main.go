package main

import (
	"log"
	"math/rand"
	"runtime"
	"time"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/glfw/v3.3/glfw"
)

const width, height = 800, 800

type Vertex struct {
	x      float32
	y      float32
	colorR float32
	colorG float32
	colorB float32
}

type Triangle struct {
	verteces []Vertex
}

func (tr *Triangle) randColors() {
	for i := 0; i < len(tr.verteces); i++ {
		tr.verteces[i].colorR = rand.Float32()
		tr.verteces[i].colorG = rand.Float32()
		tr.verteces[i].colorB = rand.Float32()
	}
}

func (tr *Triangle) init() {
	tr.verteces = make([]Vertex, 3)
	tr.verteces[0] = Vertex{-0.2, -0.1, 0, 0, 0}
	tr.verteces[1] = Vertex{0.2, -0.1, 0, 0, 0}
	tr.verteces[2] = Vertex{0, -0.3, 0, 0, 0}
	tr.randColors()
}

func (tr *Triangle) draw() {
	gl.Begin(gl.TRIANGLES)
	for _, v := range tr.verteces {
		gl.Color3f(v.colorR, v.colorG, v.colorB)
		gl.Vertex2f(v.x, v.y)
	}
	gl.End()
}

func (tr *Triangle) collideWith(x, y float64) bool {
	a := (float64(tr.verteces[0].x)-x)*(float64(tr.verteces[1].y)-float64(tr.verteces[0].y)) -
		(float64(tr.verteces[1].x)-float64(tr.verteces[0].x))*(float64(tr.verteces[0].y)-y)
	b := (float64(tr.verteces[1].x)-x)*(float64(tr.verteces[2].y)-float64(tr.verteces[1].y)) -
		(float64(tr.verteces[2].x)-float64(tr.verteces[1].x))*(float64(tr.verteces[1].y)-y)
	c := (float64(tr.verteces[2].x)-x)*(float64(tr.verteces[0].y)-float64(tr.verteces[2].y)) -
		(float64(tr.verteces[0].x)-float64(tr.verteces[2].x))*(float64(tr.verteces[2].y)-y)
	return (a >= 0 && b >= 0 && c >= 0) || (a <= 0 && b <= 0 && c <= 0)
}

func (tr *Triangle) mouseButtonCallback(w *glfw.Window, button glfw.MouseButton, action glfw.Action, mods glfw.ModifierKey) {
	if button == glfw.MouseButton1 && action == glfw.Release {
		x, y := w.GetCursorPos()
		x = 2 * (x/width - 0.5)
		y = -2 * (y/height - 0.5)
		if tr.collideWith(x, y) {
			tr.randColors()
		}
	}
}

func (tr *Triangle) keyCallback(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
	if key == glfw.KeySpace && action == glfw.Press {
		for i := 0; i < len(tr.verteces); i++ {
			tr.verteces[i].y *= -1
		}
	}
}

func init() {
	runtime.LockOSThread()
	rand.Seed(time.Now().UnixNano())
}

func main() {
	if err := glfw.Init(); err != nil {
		log.Fatalln("failed to initialize glfw:", err)
	}
	defer glfw.Terminate()

	glfw.WindowHint(glfw.Resizable, glfw.False)
	window, err := glfw.CreateWindow(width, height, "Lab1", nil, nil)
	if err != nil {
		log.Fatal(err.Error())
	}
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		log.Fatal(err.Error())
	}

	var tr, tr2 Triangle
	tr.init()
	tr2.init()
	tr2.verteces[0] = Vertex{-0.2, 0.1, 0, 0, 0}
	tr2.verteces[1] = Vertex{0.2, 0.1, 0, 0, 0}
	tr2.verteces[2] = Vertex{-0.2, 0.3, 0, 0, 0}
	tr2.randColors()

	window.SetMouseButtonCallback(glfw.MouseButtonCallback(func(w *glfw.Window, button glfw.MouseButton, action glfw.Action, mods glfw.ModifierKey) {
		tr.mouseButtonCallback(w, button, action, mods)
		tr2.mouseButtonCallback(w, button, action, mods)
	}))
	window.SetKeyCallback(glfw.KeyCallback(func(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
		tr.keyCallback(w, key, scancode, action, mods)
		tr2.keyCallback(w, key, scancode, action, mods)
	}))

	for !window.ShouldClose() {
		glfw.PollEvents()

		gl.Clear(gl.COLOR_BUFFER_BIT)
		tr.draw()
		tr2.draw()
		window.SwapBuffers()

	}

}
