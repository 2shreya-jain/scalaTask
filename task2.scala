  package com.rockthejvm

  import scala.collection.mutable.ListBuffer
  import scala.collection.mutable.Queue
  import scala.collection.mutable.Set

  object task2 extends App {
    class State(var cannibalLeft: Int, var missionaryLeft: Int, var boat: String, var cannibalRight: Int, var missionaryRight: Int) {
      var parent: State = null

      def is_goal(): Boolean = {
        if (cannibalLeft == 0 && missionaryLeft == 0) true
        else false
      }

      def is_valid(): Boolean = {
        if (missionaryLeft >= 0 && missionaryRight >= 0
          && cannibalLeft >= 0 && cannibalRight >= 0
          && (missionaryLeft == 0 || missionaryLeft >= cannibalLeft)
          && (missionaryRight == 0 || missionaryRight >= cannibalRight)) true
        else false
      }

      def _eq_(): Boolean = (this.cannibalLeft == cannibalLeft && this.missionaryLeft == missionaryLeft
        && this.boat == boat && this.cannibalRight == cannibalRight
        && this.missionaryRight == missionaryRight)
    }

    def Successors(cur_state: State): List[State] = {
      var children = ListBuffer.empty[State]
      if (cur_state.boat == "left") {
        var new_state = new State(cur_state.cannibalLeft, cur_state.missionaryLeft - 2,
          "right", cur_state.cannibalRight, cur_state.missionaryRight + 2)

        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += new_state
        }
        new_state = new State(cur_state.cannibalLeft - 2, cur_state.missionaryLeft,
          "right", cur_state.cannibalRight + 2, cur_state.missionaryRight)

        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += new_state

        }
        new_state = new State(cur_state.cannibalLeft - 1, cur_state.missionaryLeft - 1,
          "right", cur_state.cannibalRight + 1, cur_state.missionaryRight + 1)


        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += (new_state)
        }
        new_state = new State(cur_state.cannibalLeft, cur_state.missionaryLeft - 1,
          "right", cur_state.cannibalRight, cur_state.missionaryRight + 1)

        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += (new_state)
        }
        new_state = new State(cur_state.cannibalLeft - 1, cur_state.missionaryLeft,
          "right", cur_state.cannibalRight + 1, cur_state.missionaryRight)

        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += (new_state)
        }
      }
      else {
        var new_state = new State(cur_state.cannibalLeft, cur_state.missionaryLeft + 2,
          "left", cur_state.missionaryRight, cur_state.missionaryRight - 2)

        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += (new_state)
        }
        new_state = new State(cur_state.cannibalLeft + 2, cur_state.missionaryLeft,
          "left", cur_state.cannibalRight - 2, cur_state.missionaryRight)

        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += (new_state)
        }
        new_state = new State(cur_state.cannibalLeft + 1, cur_state.missionaryLeft + 1,
          "left", cur_state.cannibalRight - 1, cur_state.missionaryRight - 1)


        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += (new_state)
        }
        new_state = new State(cur_state.cannibalLeft, cur_state.missionaryLeft + 1,
          "left", cur_state.cannibalRight, cur_state.missionaryRight - 1)


        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += (new_state)
        }
        new_state = new State(cur_state.cannibalLeft + 1, cur_state.missionaryLeft,
          "left", cur_state.cannibalRight - 1, cur_state.missionaryRight)


        if (new_state.is_valid()) {
          new_state.parent = cur_state
          children += (new_state)
        }


      }

      children.toList

    }


    def breath_first_search(): State = {
      var initial_state = new State(3, 3, "left", 0, 0)
      if (initial_state.is_goal()) return initial_state

      val frontier = Queue[State]()
      var explored: Set[State] = Set.empty[State]

      frontier.enqueue(initial_state)

      while (frontier.nonEmpty) {
        val state = frontier.dequeue()
        if (state.is_goal()) return state
        explored = explored + initial_state
        var children: Seq[State] = Successors(state)
        for (child <- children) {
          if (explored.contains(child) || frontier.contains(child)) 0
          else frontier.enqueue(child)
        }
      }
      return initial_state
    }


    def print_solution(solution: State) = {
      var path = scala.collection.mutable.ListBuffer.empty[State]
      path += solution
      var parent: State = solution.parent
      while (parent != null) {
        path += parent
        parent = parent.parent
      }
      var len: Int = path.length
      while (len != 0) {
        var i = 0
        val state = path(len - i - 1)
        println(s"(${state.cannibalLeft} , ${(state.missionaryLeft)} , ${state.boat} , ${(state.cannibalRight)} , ${(state.missionaryRight)} )")
        len = len - 1
        i = i + 1

      }
    }

    var solution = breath_first_search()
    println("Missionaries and Cannibals solution:")
    println("(cannibalLeft,missionaryLeft,boat,cannibalRight,missionaryRight)")
    print_solution(solution)


  }

