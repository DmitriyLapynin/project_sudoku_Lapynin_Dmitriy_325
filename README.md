# Судоку 
Правила игры: Игровое поле представляет собой квадрат размером 9×9, разделённый на меньшие квадраты со стороной в 3 клетки. В клетках уже в начале игры стоят некоторые числа (от 1 до 9), называемые подсказками. От игрока требуется заполнить свободные клетки цифрами от 1 до 9 так, чтобы в каждой строке, в каждом столбце и в каждом малом квадрате 3×3 каждая цифра встречалась бы только один раз.

# Используемые модули 
Main - запускает весь проект;

Type - в этом модуле описаны все используемые типы;

Field - модуль для работы с полем;

Graphic - модуль для отрисовки игрового поля, чисел;

Check - модуль проверки входного файла;

Gameplay - модуль обработки событий и запуска игры.

# Используемые библиотеки
Gloss, Data.List

# Игровой процесс
В игре используются левая и правая кнопки мыши, а также кавиши f1, f2, ..., f9 для отображения цифр. 

Левой кнопкой мыши можно выделить клетку зелёным цветом, чтобы зарезервировать её для новой цифры. 

Если на зелёную клетку нажать ещё раз левой кнопкой мыши, то она снова станет пустой. 

Если на заполненную клетку(не константную) нажать правой кнопкой мыши, то она станет пустой. 

Чтобы добавить число, нужно выделить клетку зелёным цветом(левой кнопкой мыши), а после нажать 
одну из клавиш f1-f9, чтобы отобразить, соответственно, 1-9. 

