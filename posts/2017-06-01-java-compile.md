---
title: Java, сборка программы из нескольких файлов
tags: java, lua, bash
---
Как показала практика, скрипт написанный мной при
[первом знакомстве с джавой](https://vojiranto.github.io/posts/2017-05-29-java_Hello_world.html) насколько прост, настолько
же бесполезен. В частности он не позволяет собирать
программы состоящие из более чем одного файла, что есть
крупный недостаток, так как затрудняет переиспользование кода.

О том как собирать java программы из консоли можно
прочитать <a href="https://habrahabr.ru/post/125210/" target="_blank">здесь</a>.
В целом всё понятно, единственный момент, что всё это
делать руками сотни раз не наш метод.

Напишем новый скрипт! Он будет поддерживать два режима.
Выбор между ними будем осуществлять по ключу.

jc


```
#!/bin/bash
CONFIG=`jc.lua $1 $2`
case $1 in
-c)
javac $CONFIG
;;
-e)
java $CONFIG
;;
*)
esac
```

jc.lua


```
#!/usr/bin/lua5.3
dofile ("./config.lua")

local package   = string.gsub(
    config[arg[2]].package,
    "%.", "/")
local main_path = package .. "/" .. config[arg[2]].class

-- запуск компиляции.
if      arg[1] == "-c" then
    print ("-sourcepath ./src -d bin src/" .. 
        main_path .. ".java")

-- запуск исполнения.
elseif  arg[1] == "-e" then
	print ("-classpath ./bin " .. main_path)
end
```

Но теперь кроме самой программы, нужно писать небольшой
конфиг, где должны быть указаны имя название пакета и
имя класса содержащего метод main.

config.lua

```
config = {}
config.HelloWorld = {
    package = "helloWorld";
    class   = "HelloWorld";
}
```
