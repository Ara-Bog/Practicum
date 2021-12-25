# изменение рабочей директории
setwd("C:/Users/Vedroid/Desktop/FU/R/shop") 

# константы
SUPPLY <- 1
SALE <- 2

# расширения файлов поставок/продаж
FILE_SUPPLY <- 'in'
FILE_SALE <- 'out'

# изменение количества магазинов
SHOP_COUNT = 10

# изменение кол-ва дней
DAYS = 7

# название нашего магазина
SHOP_NAME = 'Пятерочка'

# список продаваемых товаров, пределы разброса значений при последующей
# генерации и цены покупки, продажи и списания товаров
GOODS <- list(
  list(name = 'Молоко, уп.', min = 100, max = 200, p_supply = 35, p_sale = 115, p_util = 35),
  list(name = 'Кефир, уп.', min = 100, max = 150, p_supply = 32, p_sale = 95, p_util = 20),
  list(name = 'Хлеб, шт.', min = 20, max = 50, p_supply = 5, p_sale = 35, p_util = 3),
  list(name = 'Вода, бут.', min = 300, max = 400, p_supply = 3, p_sale = 55, p_util = 2),
  list(name = 'Соль, пачка', min = 20, max = 50, p_supply = 8, p_sale = 55, p_util = 4),
  list(name = 'Гречка, пачка', min = 50, max = 200, p_supply = 18, p_sale = 85, p_util = 8),
  list(name = 'Торт, шт.', min = 20, max = 50, p_supply = 200, p_sale = 500, p_util = 180)
)

# по заданию нужно изменять имена столбцов после чтения,
# шаблон начальный
wrong.temp <- c('День',
                'Молоко..уп.',
                'Кефир..уп.',
                'Хлеб..шт.',
                'Вода..бут.',
                'Соль..пачка',
                'Гречка..пачка',
                'Торт..шт.')
#корректный шаблон
right.temp <- c('День недели',
                'Молоко, уп.',
                'Кефир, уп.',
                'Хлеб, шт.',
                'Вода, бут.',
                'Соль, пачка',
                'Гречка, пачка',
                'Торт, шт.')

# функция генерации данных 
# way - директория (по умолчанию текущая)
# days - количество дней
# goods - товары
# type - тип генерации (приход/расход)
# sale.level - уровень продаж в процентах за период
generate.data <- 
  function(way = '',
           days,
           goods,
           type,
           sale.level = NULL){
    if (way != ''){
      # проверка наличия директории
      isFoundDir <- dir.exists(way)
      if (isFoundDir == FALSE){
        # пытаемся создать директорию
        dir.create(path = way, showWarnings = FALSE)
        isFoundDir <- dir.exists(way)
        if (isFoundDir == FALSE){
          print('Папка назначения не существует, создать её нельзя!')
          return(NULL)
        }
      }
    }
    tab1 <- data.frame('День' = 1:days)
    if (type == SALE){
      # если у нас продажи
      # проверка на наличие поставок
      file.in <- paste0(way, SHOP_NAME,'.', FILE_SUPPLY)
      isFoundFile <- file.exists(file.in)
      if (isFoundFile) {
        data.in <- read.table(file = file.in,
                              header = TRUE,
                              sep=";",
                              encoding = 'UTF-8')
      } else {
        # если поставок нет - они создаются
        data.in <-
          generate.data(way = way,
                        days = days,
                        goods = goods,
                        type = SUPPLY)
      }
      
      # при заданном % продаж - расчет кол-ва продаж
      if (!is.null(sale.level)){
        tab1 <- cbind(tab1, sapply(data.in[1:length(goods)+1], function(x){
          return (as.integer(x*sale.level/100))
        }))
      } else {
        # иначе - генерируем рандомно
        tab1 <- cbind(tab1, sapply(data.in[1:length(goods)+1], function(x){
          return (sapply(x, function(y){
            return (sample(1:y, 1))
          }))
        }))
      }
      
    } else {
      # если у нас поставки
      tab1 <- cbind(tab1, sapply(goods, function(x){
        row <- data.frame(sample(x$min:x$max, size = days, replace = TRUE))
        colnames(row) <- x$name
        return (row)
      }))
    }
    
    ext <- switch (type, FILE_SUPPLY, FILE_SALE)
    
    write.table(
      x = tab1,
      sep=";",
      file = paste0(way, SHOP_NAME, '.', ext),
      col.names = TRUE,
      row.names = FALSE,
      fileEncoding = 'UTF-8'
    )
    return(tab1)
  }

# генерация файла с ценами
generate.price <- function(goods){
  price.frame <- data.frame(sapply(goods, function(x) {
    row <- data.frame(c(x$p_supply, x$p_sale, x$p_util))
    colnames(row) <- x$name
    return(row)
  }), row.names = c('цена поставки', 'цена продажи', 'цена утилизации'))
  
  # костыль, чтобы преобразовать названия столбцов к норм. виду
  # при создании сложенного data.frame данные отображаются корректно
  # но при объединении их - запись почему-то меняется
  colnames(price.frame)[colnames(price.frame) == wrong.temp[-1]] <- right.temp[-1]
  write.table(x = price.frame,
              file = 'Анализ/цены.txt',
              col.names = TRUE,
              row.names = TRUE,
              sep = ";",
              fileEncoding = 'UTF-8')
}

# создание файлов и папок с сгенерированными данными
generate.base <- function(goods,
                          shop.count = SHOP_COUNT,
                          sale.level = NULL,
                          days){
  dir.create(path = 'Анализ', showWarnings = FALSE)
  
  generate.price(goods = goods)
  
  sapply(c(1:shop.count), function(x) {
    way <- paste0('Магазин ', x, '/')
    dir.create(path = way, showWarnings = FALSE)
    isFoundDir <- dir.exists(way)
    if (isFoundDir == FALSE){
      print('Папка назначения не существует, создать её нельзя!')
      return(NULL)
    }
    sapply(c(SUPPLY, SALE), function(x) {
      generate.data(way = way,
                    days = days,
                    goods = goods,
                    type = x,
                    sale.level = sale.level)
      ext <- switch (x, FILE_SUPPLY, FILE_SALE)
      way.other <- paste0('Анализ/', gsub(" ", "", substr(way, 1, nchar(way)-1)), '_',SHOP_NAME, '.',ext) 
      isFoundFile <- file.exists(way.other)
      if (isFoundFile){
        file.remove(way.other)
      }
      file.copy(paste0(way, SHOP_NAME, '.',ext), way.other)
    })
    
    
  })
}

# генерация исходных данных
generate.base(goods = GOODS, days = DAYS)

# считывание данных по всем магазинам
data.reader <- function(){
  data.all <- sapply(c(1:SHOP_COUNT), function(x) {
    shop.in <- 
      data.frame(read.table(file = paste0('Анализ/Магазин',x,"_",SHOP_NAME,'.in'),
                            header = TRUE,
                            sep=';',
                            encoding = 'UTF-8'))
    shop.out <- 
      data.frame(read.table(file = paste0('Анализ/Магазин',x,"_",SHOP_NAME,'.out'),
                            header = TRUE,
                            sep=';',
                            encoding = 'UTF-8'))
    return(list(list('in' = shop.in, 'out' = shop.out)))
  })
  return(data.all)
}

#обработка данных из таблицы в csv файлы
create.table.csv <- function(data.all, price.data){
  data.caption <- c('Выручка, руб.', 'Прибыль, руб', 'Реализация, шт', 'Списание, шт', 
                    'Равномерность продаж', 'Продажи макс', 'День макс продаж', 'Продажи мин',
                    'День мин продаж', 'Списание макс', 'День макс списания')
  
  data.total <- data.frame(t(sapply(data.all, function(x) {
    patten <- data.frame(x$'in'[1])
    
    shop.in.price <- cbind(patten, data.frame(x$'in'[2:length(x$'in')] * price.data[1,][col(x$'in') - 1]))
    shop.out.price <- cbind(patten, data.frame(x$'out'[2:length(x$'out')] * price.data[2,][col(x$'out') - 1]))
    rest <- cbind(patten, data.frame(x$'in'[2:length(x$'in')] - x$'out'[2:length(x$'out')]))
    
    TR <- sum(shop.out.price)
    TC <- sum(shop.in.price)
    Pr <- TR - TC
    
    Q_util <- sum(rest[2:length(rest)])
    Q_sale <- sum(x$'out'[2:length(x$'out')])
    sum.day.sale <- rowSums(x$'out'[2:length(x$'out')])
    sum.day.util <- rowSums(rest[2:length(rest)])
    
    sd.sale <-  sd(sum.day.sale)
    
    rest.money <- data.frame(x$'in'[1])
    rest.money <- cbind(patten, data.frame(rest[2:length(rest)] * price.data[3,][col(rest) - 1]))
    
    max.sale <- max(sum.day.sale)
    max.day.sale <- which.max(sum.day.sale)
    
    min.sale <- min(sum.day.sale)
    min.day.sale <- which.min(sum.day.sale)
    
    max.util <- max(sum.day.util)
    max.day.util <- which.max(sum.day.util)
    
    return (c(TR, Pr, Q_sale, Q_util, sd.sale, max.sale, max.day.sale,
              min.sale, min.day.sale, max.util, max.day.util))
  })))
  colnames(data.total) <- data.caption
  rownames(data.total) <- sapply(1:SHOP_COUNT, function(x) {
    return(paste0('Магазин ', x))
  })
  
  write.table(data.total,
              file = paste0('Анализ/Показатели', '.csv'),
              col.names = TRUE,
              row.names = TRUE,
              sep = ';',
              dec = ',',
              fileEncoding = 'UTF-8')
  return (data.total)
}

#получение данных и их обработка

price.data <- 
  read.table(file = 'Анализ/цены.txt',
             header = TRUE,
             sep=';',
             encoding = 'UTF-8'
  )
data.all <- data.reader()

data.total <- create.table.csv(data.all, price.data)


#задание с графиками 1
#для рассмотра графика задаем x-номер магазина, i- номер товара
x <- 1  
i <- 1
sale.volume <- data.all[[x]]$'out'[[i+1]]
supply.volume <- data.all[[x]]$'in'[[i+1]]
util.volume <- supply.volume - sale.volume
util.price <- util.volume*price.data[i,3]
sale.revenue <- sale.volume*price.data[i,2]
sale.profit <- sale.revenue -
  supply.volume*price.data[i,1] - 
  util.price

profitability <- sale.profit/sale.revenue*100

all.lines <-
  list('объем продаж' = sale.volume,
       'выручка' = sale.revenue,
       'прибыль' = sale.profit,
       'списание' = util.price,
       'рентабельность' = profitability)
cls.lst = colors()[c(17,115, 54, 43, 98)]
par(mfrow = c(2,3))
sapply(c(1:length(all.lines)), function(y) {
  plot(all.lines[[y]],
       type = "b",
       col = cls.lst[y],
       sub = names(data.all[[x]]$'out')[i+1],
       xlab = "день",
       ylab = "Показатель",
       main = names(all.lines)[y]
  )
})

#задание по графикам 2


x <- 1
all.lines <-
  list('прибыль' = list(),
       'списание' = list(),
       'рентабельность' = list())


cls.lst = colors()[sample(1:length(colors()),(length(data.all[[x]]$'in')-1) , replace = FALSE)]

for (i in 1:(length(data.all[[x]]$'in')-1)){
  
  sale.volume <- data.all[[x]]$'out'[[i+1]]
  supply.volume <- data.all[[x]]$'in'[[i+1]]
  util.volume <- supply.volume - sale.volume
  util.price <- util.volume*price.data[3, i]
  sale.revenue <- sale.volume*price.data[2, i]
  sale.profit <- sale.revenue -
    supply.volume*price.data[1, i] - 
    util.price
  
  profitability <- sale.profit/sale.revenue*100
  
  all.lines$'прибыль'[[names(data.all[[x]]$'out')[i+1]]] <- sale.profit
  all.lines$'списание'[[names(data.all[[x]]$'out')[i+1]]] <- util.volume
  all.lines$'рентабельность'[[names(data.all[[x]]$'out')[i+1]]] <- profitability
}

par(mfrow = c(1,3))
for (j in 1:length(all.lines)){
  max_ <- 0
  min_ <- 0
  for (el in all.lines[[j]]){
    if (max(el)>max_){
      max_ <- max(el)
    }
    if(min(el)<min_){
      min_ <- min(el)
    }
  }
  plot(0,0,
       type = "n",
       ylim = c(min_, max_),
       xlim = c(1,7),
       sub = names(data.all[[x]]$'out')[i+1],
       xlab = "день",
       ylab = "Показатель",
       main = names(all.lines)[j]
  )
  
  for (i in 1:length(all.lines[[j]])){
    
    lines(all.lines[[j]][[i]],
          type = "b",
          ylim = c(min(unlist(all.lines[[j]])),max(unlist(all.lines[[j]]))),
          sub = names(data.all[[x]]$'out')[i+1],
          col = cls.lst[i],
          xlab = "день",
          ylab = "Показатель",
          main = names(all.lines)[j]
    )
  }
}

legend(x = "bottomright",
       legend = names(all.lines[[1]]),
       lty = c(1),
       col = cls.lst,
       lwd = 4)


#задание с графиками 3
#продукт i
par(mfrow = c(1,1))
i <- 3
volume.lst <- c()
cls.lst = colors()[sample(1:length(colors()),length(data.all) , replace = FALSE)]

for (x in 1:length(data.all)){
  volume.lst <- c(volume.lst, sum(data.all[[x]]$'out'[[i+1]]))
}

pie(volume.lst,
    labels = volume.lst,
    main = "Объем продаж по магазинам",
    col = cls.lst)
print(names(data.all))
legend("topright",
       legend = names(data.all),
       lty = c(1),
       col = cls.lst,
       lwd = 3)
