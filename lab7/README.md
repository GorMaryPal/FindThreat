# Анализ данных сетевого трафика при помощи библиотеки Arrow
Горбачева Мария Павловна

## Цель работы

1.  Изучить возможности технологии Apache Arrow для обработки и анализ
    больших данных
2.  Получить навыки применения Arrow совместно с языком программирования
    R
3.  Получить навыки анализа метаинфомации о сетевом трафике
4.  Получить навыки применения облачных технологий хранения, подготовки
    и анализа данных: Yandex Object Storage, Rstudio Server.

## Исходные данные

1.  Программное обеспечение Windows 10
2.  Rstudio Desktop и библиотека Dplyr
3.  Интерпретатор яз/\*ыка R 4.1
4.  Сервис Yandex DataLens
5.  Apache Arrow

## Задание

Используя язык программирования R, библиотеку arrow и облачную IDE
Rstudio Server, развернутую в Yandex Cloud, выполнить задания и
составить отчет.

## План

1.  Выполним практическое задание
2.  Составим отчет

## Ход работы

### Импорт данных

``` r
library(arrow)
```

    Warning: пакет 'arrow' был собран под R версии 4.4.2


    Присоединяю пакет: 'arrow'

    Следующий объект скрыт от 'package:utils':

        timestamp

``` r
library(tidyverse)
```

    Warning: пакет 'tidyverse' был собран под R версии 4.4.2

    Warning: пакет 'ggplot2' был собран под R версии 4.4.2

    Warning: пакет 'tidyr' был собран под R версии 4.4.2

    Warning: пакет 'readr' был собран под R версии 4.4.2

    Warning: пакет 'purrr' был собран под R версии 4.4.2

    Warning: пакет 'forcats' был собран под R версии 4.4.2

    Warning: пакет 'lubridate' был собран под R версии 4.4.2

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ lubridate::duration() masks arrow::duration()
    ✖ dplyr::filter()       masks stats::filter()
    ✖ dplyr::lag()          masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
#download.file('https://storage.yandexcloud.net/arrow-datasets/tm_data.pqt', destfile = "tm_data.pqt")
df <- read_parquet("tm_data.pqt", use_threads = FALSE)
```

### Задание 1. Найдите утечку данных из Вашей сети

Один из хостов в нашей сети используется для пересылки этой информации –
он пересылает гораздо больше информации на внешние ресурсы в Интернете,
чем остальные компьютеры нашей сети. Определите его IP-адрес

``` r
ff <- df%>%filter(grepl("^12.|^13.|^14.", src) & !grepl("^12.|^13.|^14.", dst))%>%group_by(src) %>% summarise("sum" = sum(bytes)) %>% select(src,sum) 
ff1 <- ff%>%filter(sum>6000000000)
glimpse(ff1)
```

    Rows: 1
    Columns: 2
    $ src <chr> "13.37.84.125"
    $ sum <dbl> 10625497574

### Задание 2. Найдите утечку данных из Вашей сети 2

Другой атакующий установил автоматическую задачу в системном
планировщике cron для экспорта содержимого внутренней wiki системы. Эта
система генерирует большое количество трафика в нерабочие часы, больше
чем остальные хосты. Определите IP этой системы. Известно, что ее IP
адрес отличается от нарушителя из предыдущей задачи.

``` r
z2 <- df%>%select(timestamp, src, dst, bytes)%>%mutate(trafic=grepl("^12.|^13.|^14.", src) & !grepl("^12.|^13.|^14.",dst),time=hour(as_datetime(timestamp/1000))) %>%filter(trafic==TRUE,time>=0&time<=24)%>% group_by(time)%>%summarise(trafictime=n())%>%arrange(desc(trafictime))
glimpse(z2)
```

    Rows: 24
    Columns: 2
    $ time       <int> 16, 22, 18, 23, 19, 21, 17, 20, 13, 7, 0, 3, 14, 6, 12, 10,…
    $ trafictime <int> 4490576, 4489703, 4489386, 4488093, 4487345, 4487109, 44835…

Следовательно: нерабочие часы - 1-15; рабочие часы - 16-24, тогда

``` r
nz2 <- df%>%mutate(time=hour(as_datetime(timestamp/1000)))%>%filter(!grepl("^13.37.84.125",src))%>% filter(grepl("^12.|^13.|^14.", src) & !grepl("^12.|^13.|^14.", dst))%>%filter(time>=1&time<=15)%>%group_by(src)%>%summarise("sum" =sum(bytes))%>%select(src,sum)
nz22 <- nz2%>%filter(sum>286000000) 
glimpse(nz22)
```

    Rows: 1
    Columns: 2
    $ src <chr> "12.55.77.96"
    $ sum <int> 286315073

### Задание 3. Найдите утечку данных из Вашей сети 3

Еще один нарушитель собирает содержимое электронной почты и отправляет в
Интернет используя порт, который обычно используется для другого типа
трафика. Атакующий пересылает большое количество информации используя
этот порт, которое нехарактерно для других хостов, использующих этот
номер порта. Определите IP этой системы. Известно, что ее IP адрес
отличается от нарушителей из предыдущих задач.

``` r
z3 <- df %>% filter(!grepl("^13.37.84.125",src)& !grepl("^12.55.77.96",src))%>% select(src, bytes, port) 
z33 <- z3%>%group_by(port)%>%summarise("mean"=mean(bytes),"max"=max(bytes),"sum"=sum(bytes))%>%mutate("Raz"= max-mean)%>%filter(Raz!=0, Raz>170000)
z333 <- z3%>%filter(port==37)%>%group_by(src)%>%summarise("mean"=mean(bytes))%>%filter(mean>37543)%>%select(src)
glimpse(z333)
```

    Rows: 33
    Columns: 1
    $ src <chr> "13.46.35.35", "15.110.113.94", "15.35.40.26", "15.36.118.82", "15…

Ответ: 13.46.35.35

## Оценка результата

Используя язык программирования R, библиотеку arrow и облачную IDE
Rstudio Server, развернутую в Yandex Cloud, задания были успешно
выполнены.

## Вывод

Apache Arrow - хорошее средство для решения задач с большими данными и
анализа информации о сетевом трафике
