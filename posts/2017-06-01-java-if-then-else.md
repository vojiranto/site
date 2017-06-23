---
title: Переиспользование кода
tags: java
---
Следующий раздел задачника посвящён линейным операторам,
однако вместо этого потренеруюсь к сведению одних задач
к другим и переиспользованию кода. Для этого, а также
для возможности автоматизации тестирования реализую
задачи не в качестве отдельных программ, а в виде
отдельных методов.

Для повышения же удобства разобрался как компилировать
из нескольких файлов, дальше в этом направлении сборка
отдельных пакетов и использование уже собранных, но это
пока подождёт.

Задача 6 --- нахождение максимума из двух чисел, а
задача 7 --- из трёх. Очевидным образом задача 7
сводится к задаче шесть. Воспользуюсь этим для
сокращения писанины.

```
package numFunc;
import java.util.Scanner;

public class NumMax {
    // Задача 6.
    private static int max (int a, int b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    // Задача 7.
    private static int max3 (int a, int b, int c) {
        return max(max(a, b), c);
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        // Ввод чисел.
        int a = scanner.nextInt();
        int b = scanner.nextInt();
        int c = scanner.nextInt();

        System.out.println(max(a, b));
        System.out.println(max3(a, b, c));
    }
}
```

Задача 8 --- просто упражнение на умение пользоваться
case'ом.

```
import java.util.Scanner;

public class DaysOfTheWeek {
    private static String day (int num) {
        switch (num) {
            case 1:  return "Monday";
            case 2:  return "Tuesday";
            case 3:  return "Wednesday";
            case 4:  return "Thursday";
            case 5:  return "Friday";
            case 6:  return "Saturday";
            case 7:  return "Sunday";
            default: return "Error";
        }
    }
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        int num = scanner.nextInt();

        System.out.println(day(num));
    }
}
```

Задачи 9 и 11 сводятся к задачке о записи числа в
обратном порядке...

```
package palindromeNum;
import java.util.Scanner;
import numFunc.NumFunc;

public class PalindromeNum {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int num = scanner.nextInt();

        // Задача 9
        System.out.println (
            NumFunc.reverseNum(num, 10) == num);
        // Задача 11
        System.out.println (
            NumFunc.reverseNum(num, 2) == num);
    }
}
```

Задача 10 - поиск корней квадратного уравнения.

```
package quadraticEquation;

import java.util.Scanner;

class QuadraticEquation {
    public static void main (String[] args) {
	    Scanner scanner = new Scanner(System.in);

        // Вводим числа
        double a = scanner.nextInt();
	    double b = scanner.nextInt();
	    double c = scanner.nextInt();

        double d = b*b - 4*a*c;

        if (d > 0) {
            double x1 = ( Math.sqrt(d) - b)/(2*a);
            double x2 = (-Math.sqrt(d) - b)/(2*a);
            System.out.println(
                "x1 = " + x1 + "; x2 = " + x2);
        }
        if (d == 0) {
            double x = -(b/(2*a));
            System.out.println("x =" + x);
        }
        if (d < 0) {
            System.out.println("No real solutions");
        }
    }
}
```

В целом, как разминка не плохо, но задачи этого раздела
почти бесполезны.
