# Задания

## Для выполнения заданий
Отпочкуйте от `main` ветку `solutions`, в которую потом будете делать пулл реквесты с домашними заданиями

## Как выполнять задания
Перед выполнением каждого (!) задания
1. Ребейзните ветку `solutions` вашего форка на `master` ветку этого репозитория
2. Отпочкуйте ветку c именем начинающимся на `solution-${номер семестра (s1/s2)}-${номер задания, для однозначных начиная с 0}`
   *Например, для 1-го задания 1-го семестра имя ветки будет `solution-s1-01`*
3. Реализуйте домашнее задание в этой ветке в соответствующем модуле проекта
4. Проверьте, что
   - тесты проходят успешно с помощью `sbt` команды `hw` (определена в `build.sbt`)
   - код отформатирован c помощью команды `scalafmtCheckAll`
5. Сделайте пулл реквест в ветку `solutions`.
   Убедитесь, что пайплайн зеленый и отправьте ментору на проверку через edu.
   Если ваш пайплайн красный, но вы уверены, что тесты успешно проходят, отправляйте задание на проверку через edu и пишите о проблемах в телеграм чат.

## Задания

- [s1-01](/modules/s1-01-scala-intro/src/main/scala/hw/Task01.scala)
- [s1-02](/modules/s1-02-adts/src/main/scala/hw)
- [s1-03](/modules/s1-03-collections/src/main/scala/hw)
- [s1-04](/modules/s1-04-laziness-errors/src/main/scala/hw)
- [s1-05](/modules/s1-05-type-classes/src/main/scala/hw)
- [s1-06](/modules/s1-06-concurrency/src/main/scala/hw)
- [s1-07](/modules/s1-07-effects-basic/src/main/scala)
- [s1-08](/modules/s1-08-effects-di/src/main/scala)
- [s1-09](/modules/s1-09-functor-flatmap/main/scala/ru/mipt/fp/README.md)
