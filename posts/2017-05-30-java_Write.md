---
title: Java чтение из консоли
tags: java
---
Продолжаю знакомиться с jav'ой. В качестве рукаводстава решил использовать 
какой-нибудь задачник. Выбрал <a href="http://www.studfiles.ru/preview/5911339/">вот этот</a>.
Задачки простые, но их много и для базового ознакомления подходят.

Вот ими и буду руководствоваться. После <q>HelloWorld</q> там идёт задача на
считывание чисел и их печать в обратном порядке.

> **Формулировка.** Вывести на экран три введенных с клавиатуры числа в порядке,
> обратном их вводу.

Алгоритм элементарный и дополнительных пояснений не требует. Единственный нюанс
связан с тем, как организован ввод в java. Это можно вычитать <a href="http://pages.cs.wisc.edu/~hasti/cs367/resources/JavaIO/JavaIO.html" target="_blank">здесь</a>.

```
import java.util.Scanner;

public class WriteThree {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int a = scanner.nextInt();
        int b = scanner.nextInt();
        int c = scanner.nextInt();
        System.out.println(c + " " + b + " " + a);
    }
}
```
