#lang pyret

import "check.arr" as Check

provide {
  class: class,
  Objekt: Objekt
} end

# The base object for all hierarchies
object-brander = brander()
Objekt = object-brander.brand({
  #_brander: brander(),
  new(self, spec): object-brander.brand({
    get(_, name): raise("get: field not found: ".append(name)) end,
    set(_, name, v): raise("set: field not found: ".append(name)) end,
    invoke(_, name, a): raise("invoke: method not found: ".append(name)) end,
    instance-of(_, klass): object-brander.test(klass) end,
    view-as(inst, klass):
      if object-brander.test(klass):
        inst
      else:
        raise("Incompatible cast in view-as")
      end
    end
  }) end,
  ext(self, ext-descr): ext(self, ext-descr) end,
})

# : Class -> ClassDescription -> Class
fun ext(parent-class, description):
  class-brander = brander()
  class-brander.brand({

    # : (Class) -> Objekt -> Instance
    new(self, spec):
      var fields = description.fields
      methods = description.methods
      var parent-inst = nothing # to be init'd by super from constructor

      instance = {

        # : (Instance) -> String -> Any
        get(_, name):
          if builtins.has-field(fields, name):
            fields.[name]
          else:
            parent-inst.get(name)
          end
        end,

        # : (Instance) -> String -> Any -> Any
        set(_, name, val):
          if builtins.has-field(fields, name):
            fields := fields.{ [name]: val }
          else:
            parent-inst.set(name, val)
          end
        end,

        # : (Instance) -> String -> Any -> Any
        # For now, only support one arg methods
        invoke(inst, name, arg):

          inst-with-super-inner = inst.{
            super(inst-inner, arg-inner):
              parent-inst:invoke._fun()(inst-inner.view-as(parent-class), name, arg-inner)
            end
          }

          if builtins.has-field(methods, name):
            # NOTE(joe 26 Jul 2013): Horrible parser ambiguity requires these parens
            (methods:[name]._fun()(inst-with-super-inner, arg))
          else:
            parent-inst:invoke._fun()(inst.view-as(parent-class), name, arg)
          end
        end,

        # : (Instance) -> Class -> Bool
        instance-of(_, klass):
          class-brander.test(klass) or parent-inst.instance-of(klass)
        end,

        view-as(inst, klass):
          if class-brander.test(klass):
            inst
          else:
            parent-inst:view-as._fun()(inst.{
                get: parent-inst:get,
                set: parent-inst:set,
                invoke(_, name, arg):
                  inst.invoke(name, arg)
                end
              }, klass)
          end
        end
      }

      inst-with-super = instance.{
        super(inst, spec-inner):
          parent-inst := parent-class.new(spec-inner)
          inst
        end
      }

      inst-constructed = description:constructor._fun()(inst-with-super, spec)
      #drop-fields(inst-constructed, ["super"])
      inst-constructed
    end,

    # : (Class) -> ClassDescription -> Class
    ext(self, ext-descr): ext(self, ext-descr) end,
  })
where:
  # Tests

  todo-class-descr = {
    fields: {
      due: "String",
      task: "String",
      done: "Boolean"
    },
    methods: {
      is-completed(self, _): self.get("done") end,
      complete(self, _):
        self.set("done", true) end
    },
    # Constructor should return an object to use as self
    # : (Instance) -> Objekt -> Instance
    constructor(self, spec):
      self.set("due", spec.due)
      self.set("task", spec.task)
      self.set("done", false)
      self.super(spec)
    end
  }

  assignee-ext-descr = {
    fields: {
      assignee: "String"
    },
    methods: {

      assign(self, person):
        if self.get("done"):
          raise("Can't assign a completed task")
        else:
          self.set("assignee", person)
        end
      end,

      complete(self, o):
        if is-nothing(self.get("assignee")):
          raise("Can't complete an unassigned task")
        else:
          self.super(o)
        end
      end
    },
    constructor(self, spec):
      self.set("assignee", nothing)
      self.super(spec)
    end
  }

  Todo = class(todo-class-descr)
  todo1 = Todo.new({ due: "Feb 2", task: "do that thing"})

  checkers.check-equals("get task", todo1.get("task"), "do that thing")
  todo1.set("task", "make some java")
  checkers.check-equals("get task after set", todo1.get("task"), "make some java")

  checkers.check-equals("get done", todo1.get("done"), false)
  todo1.invoke("complete", nothing)
  checkers.check-equals("get done after invoke", todo1.get("done"), true)

  checkers.check-true("instance-of", todo1.instance-of(Todo))

  AssignableTodo = Todo.ext(assignee-ext-descr)
  todo2 = AssignableTodo.new({ due: "Feb 8", task: "assign someone" })

  checkers.check-equals("get child field", todo2.get("assignee"), nothing)
  checkers.check-equals("get parent field", todo2.get("due"), "Feb 8")

  todo2.set("assignee", "Joe")
  checkers.check-equals("set child field", todo2.get("assignee"), "Joe")
  todo2.set("due", "Feb 9")
  checkers.check-equals("set parent field", todo2.get("due"), "Feb 9")

  checkers.check-true("instance-of-child", todo2.instance-of(AssignableTodo))
  checkers.check-true("instance-of-parent", todo2.instance-of(Todo))
  checkers.check-true("instance-of Objekt", todo2.instance-of(Objekt))

  todo2.invoke("assign", "Jonah")
  checkers.check-equals("invoke child method", todo2.get("assignee"), "Jonah")
  todo2.invoke("is-completed", nothing)
  checkers.check-false("invoke parent method", todo2.get("done"))

  todo2.invoke("complete", nothing)
  checkers.check-true("invoke overridden method", todo2.get("done"))
end


# Don't really need this...
fun class(description): Objekt.ext(description) end
