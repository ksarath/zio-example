package zio.example.todo.repository

import zio.example.todo.{TodoId, TodoItem, TodoItemPatchForm, TodoItemPostForm}
import zio._
import zio.delegate.Mix
import zio.example.todo.{TodoId, TodoItem, TodoItemPatchForm, TodoItemPostForm}

final class InMemoryTodoRepository(ref: Ref[Map[TodoId, TodoItem]], counter: Ref[Long])
    extends TodoRepository.Service[Any] {

  override def getAll(): ZIO[Any, Nothing, List[TodoItem]] =
    ref.get.map(_.values.toList)

  override def getById(id: TodoId): ZIO[Any, Nothing, Option[TodoItem]] =
    ref.get.map(_.get(id))

  override def delete(id: TodoId): ZIO[Any, Nothing, Unit] =
    ref.update(store => store - id).unit

  override def deleteAll: ZIO[Any, Nothing, Unit] =
    ref.update(_.empty).unit

  override def create(todoItemForm: TodoItemPostForm): ZIO[Any, Nothing, TodoItem] =
    for {
      newId <- counter.update(_ + 1).map(TodoId)
      todo  = todoItemForm.asTodoItem(newId)
      _     <- ref.update(store => store + (newId -> todo))
    } yield todo

  override def update(id: TodoId, todoItemForm: TodoItemPatchForm): ZIO[Any, Nothing, Option[TodoItem]] =
    for {
      oldValue <- getById(id)
      result <- oldValue.fold[UIO[Option[TodoItem]]](ZIO.succeed(None)) { x =>
                 val newValue = x.update(todoItemForm)
                 ref.update(store => store + (newValue.id -> newValue)) *> ZIO.succeed(Some(newValue))
               }
    } yield result

}

object InMemoryTodoRepository {

  def withInMemoryRepository[R](ref: Ref[Map[TodoId, TodoItem]], counter: Ref[Long])(
    implicit ev: R Mix TodoRepository
  ): ZIO[R, Nothing, R with TodoRepository] =
    ZIO
      .environment[R]
      .map(
        r =>
          ev.mix(r, new TodoRepository {
            val todoRepository = new InMemoryTodoRepository(ref, counter)
          })
      )

  def withInMemoryRepositoryManaged[R](ref: Ref[Map[TodoId, TodoItem]], counter: Ref[Long])(
    implicit ev: R Mix TodoRepository
  ): ZManaged[R, Nothing, R with TodoRepository] =
    withInMemoryRepository(ref, counter).toManaged_
}
