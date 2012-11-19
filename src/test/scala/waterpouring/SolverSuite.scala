package week7.waterpouringByAlexi

import org.scalatest.FunSuite
import Solver._

class SolverSuite extends FunSuite {

  test("0 moves") {
    val solver = new Solve(Vector(1), 0)
    expect(Set(Nil)) {
      solver.solve
    }
  }

  test("1 move") {
    val solver = new Solve(Vector(4), 4)
    expect(Set(List(Fill(0)))) {
      solver.solve
    }
  }

  test("2 moves") {
    val solver = new Solve(Vector(4, 1), 3)
    expect(Set(List(Fill(0), Pour(0, 1)))) {
      solver.solve
    }
  }

  test("original assignment") {
    val solver = new Solve(Vector(4, 9), 6)
    expect(Set(List(
      Fill(1), // 0 9
      Pour(1,0), // 4 5
      Empty(0), // 0 5
      Pour(1,0), // 4 1
      Empty(0), // 0 1
      Pour(1,0), // 1 0
      Fill(1), // 1 9
      Pour(1,0) // 4 6
    ))) {
      solver.solve
    }
  }

  test("doMove: Fill") {
    val solver = new Solve(Vector(3, 2, 1), 3)
    expect(Vector(3, 0, 0)) {
      solver.doMove(solver.initialState, Fill(0))
    }
    expect(Vector(0, 2, 0)) {
      solver.doMove(solver.initialState, Fill(1))
    }
    expect(Vector(0, 0, 1)) {
      solver.doMove(solver.initialState, Fill(2))
    }
  }

  test("doMove: Empty") {
    val solver = new Solve(Vector(3, 2, 1), 3)
    expect(Vector(0, 2, 3)) {
      solver.doMove(Vector(1,2,3), Empty(0))
    }
    expect(Vector(1, 0, 3)) {
      solver.doMove(Vector(1,2,3), Empty(1))
    }
    expect(Vector(1, 2, 0)) {
      solver.doMove(Vector(1,2,3), Empty(2))
    }
  }

  test("doMove: Pour") {
    val solver = new Solve(Vector(3, 2, 1), 3)
    expect(Vector(2, 0, 1)) {
      solver.doMove(Vector(0, 2, 1), Pour(1,0))
    }
    expect(Vector(3, 0, 1)) {
      solver.doMove(Vector(1, 2, 1), Pour(1,0))
    }
    expect(Vector(3, 1, 1)) {
      solver.doMove(Vector(2, 2, 1), Pour(1,0))
    }
    expect(Vector(3, 2, 1)) {
      solver.doMove(Vector(3, 2, 1), Pour(2,0))
    }
  }

  test("initial state") {
    expect(Vector()) {
      val solver = new Solve(Vector(), 0)
      solver.initialState
    }
    expect(Vector(0,0,0)) {
      val solver = new Solve(Vector(6,3,4), 3)
      solver.initialState
    }
  }
}
