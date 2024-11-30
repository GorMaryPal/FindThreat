# Основы обработки данных с помощью R и Dplyr
Горбачева Мария Павловна

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

## Исходные данные

1.  Программное обеспечение Windows 10
2.  Rstudio Desktop и библиотека Dplyr
3.  Интерпретатор языка R 4.1
4.  пакет dplyr

## Ход работы

### 1. Установка программного пакета dplyr.

``` r
install.packages("dplyr")
```

### 2. Загрузка библиотеки

``` r
library("dplyr")
```

    Warning: пакет 'dplyr' был собран под R версии 4.4.2


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

### 3. Сколько строк в датафрейме?

``` r
starwars %>% nrow()
```

    [1] 87

### 4. Сколько столбцов в датафрейме?

``` r
starwars %>% ncol()
```

    [1] 14

### 5. Как посмотреть примерный вид датафрейма?

``` r
starwars %>% glimpse()
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

### 6. Сколько уникальных рас персонажей (species) представлено в данных?

``` r
starwars %>% distinct(species) %>% filter(!is.na(species)) %>% nrow()
```

    [1] 37

### 7. Найти самого высокого персонажа.

``` r
starwars %>% filter(is.na(height) == FALSE) %>% slice_max(height) %>% select(name,height)
```

    # A tibble: 1 × 2
      name        height
      <chr>        <int>
    1 Yarael Poof    264

### 8. Найти всех персонажей ниже 170

``` r
starwars %>% filter(height < 170) %>% select(name,height)
```

    # A tibble: 22 × 2
       name                  height
       <chr>                  <int>
     1 C-3PO                    167
     2 R2-D2                     96
     3 Leia Organa              150
     4 Beru Whitesun Lars       165
     5 R5-D4                     97
     6 Yoda                      66
     7 Mon Mothma               150
     8 Wicket Systri Warrick     88
     9 Nien Nunb                160
    10 Watto                    137
    # ℹ 12 more rows

### 9. Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ подсчитать по формуле

![](img/1.png)

``` r
print(starwars %>% mutate(imt = mass / (height)^2) %>% select(name, imt))
```

    # A tibble: 87 × 2
       name                   imt
       <chr>                <dbl>
     1 Luke Skywalker     0.00260
     2 C-3PO              0.00269
     3 R2-D2              0.00347
     4 Darth Vader        0.00333
     5 Leia Organa        0.00218
     6 Owen Lars          0.00379
     7 Beru Whitesun Lars 0.00275
     8 R5-D4              0.00340
     9 Biggs Darklighter  0.00251
    10 Obi-Wan Kenobi     0.00232
    # ℹ 77 more rows

### 10. Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по отношению массы (mass) к росту (height) персонажей.

``` r
starwars %>% mutate(v = mass/height)%>% arrange(desc(v)) %>% select(name,v) %>% head(n=10)
```

    # A tibble: 10 × 2
       name                      v
       <chr>                 <dbl>
     1 Jabba Desilijic Tiure 7.76 
     2 Grievous              0.736
     3 IG-88                 0.7  
     4 Owen Lars             0.674
     5 Darth Vader           0.673
     6 Jek Tono Porkins      0.611
     7 Bossk                 0.595
     8 Tarfful               0.581
     9 Dexter Jettster       0.515
    10 Chewbacca             0.491

### 11. Найти средний возраст персонажей каждой расы вселенной Звездных войн

``` r
starwars %>% group_by(species) %>% 
filter(!is.na(birth_year)) %>% 
summarise(mean(birth_year))
```

    # A tibble: 15 × 2
       species        `mean(birth_year)`
       <chr>                       <dbl>
     1 Cerean                       92  
     2 Droid                        53.3
     3 Ewok                          8  
     4 Gungan                       52  
     5 Human                        53.7
     6 Hutt                        600  
     7 Kel Dor                      22  
     8 Mirialan                     49  
     9 Mon Calamari                 41  
    10 Rodian                       44  
    11 Trandoshan                   53  
    12 Twi'lek                      48  
    13 Wookiee                     200  
    14 Yoda's species              896  
    15 Zabrak                       54  

### 12. Найти самый распространенный цвет глаз персонажей вселенной Звездных войн.

``` r
starwars %>% 
filter(!is.na(eye_color)) %>%  
group_by(eye_color) %>% summarise(sum = n()) %>%
arrange(desc(sum)) %>% head(1)
```

    # A tibble: 1 × 2
      eye_color   sum
      <chr>     <int>
    1 brown        21

### 11. Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн.

``` r
starwars %>% filter(!is.na(species)) %>%
mutate(nlen=nchar(name)) %>% group_by(species) %>% summarise(mean(nlen))
```

    # A tibble: 37 × 2
       species   `mean(nlen)`
       <chr>            <dbl>
     1 Aleena           12   
     2 Besalisk         15   
     3 Cerean           12   
     4 Chagrian         10   
     5 Clawdite         10   
     6 Droid             4.83
     7 Dug               7   
     8 Ewok             21   
     9 Geonosian        17   
    10 Gungan           11.7 
    # ℹ 27 more rows

## Оценка результатов

Были развиты практические навыки использования функций обработки данных
пакета dplyr на примере дата сета “starwars”

## Вывод

Помимо базовых возжностей языка R для обработки данных, существуют
библиотеки для более комфортной и наглядной работы с данными
