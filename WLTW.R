Harga <- na.omit(as.data.frame(Willis_Towers_Watson_Stock_Price_History))
Harga_Tanggal <- ymd(Harga$Date)
Hargadf <- ((data.frame(Harga_Tanggal,Harga$Price)))
Hargats <- (xts(Harga$Price,Harga_Tanggal))
summary(Hargats)

plot_Harga <- ggplot(Harga, aes(x= Date, y = Price)) + geom_line(col="red")
plot_Harga
ggplotly(plot_Harga)

lambda <- BoxCox.lambda(Hargats)
adf.test(Hargats)
Hargafinal <- na.omit(diff(Hargats),1)
adf.test(Hargafinal)

eacf(Hargafinal)

arimaHarga1 <- arima(Hargafinal,order = c(0,1,2),method = "ML")
arimaHarga2 <- arima(Hargafinal,order = c(0,1,3),method = "ML")
arimaHarga3 <- arima(Hargafinal,order = c(2,1,0),method = "ML")
arimaHarga4 <- arima(Hargafinal,order = c(2,1,1),method = "ML")
arimaHarga5 <- arima(Hargafinal,order = c(2,1,2),method = "ML")
arimaHarga6 <- arima(Hargafinal,order = c(1,1,0),method = "ML")
arimaHarga7 <- arima(Hargafinal,order = c(3,1,0),method = "ML")
arimaHarga8 <- arima(Hargafinal,order = c(4,1,0),method = "ML")
arimaHarga9 <- arima(Hargafinal,order = c(5,1,0),method = "ML")
arimaHarga10 <- arima(Hargafinal,order = c(6,1,0),method = "ML")
arimaHarga11 <- arima(Hargafinal,order = c(7,1,0),method = "ML")
arimaHarga12 <- arima(Hargafinal,order = c(8,1,0),method = "ML")
arimaHarga13 <- arima(Hargafinal,order = c(9,1,0),method = "ML")
arimaHarga14 <- arima(Hargafinal,order = c(10,1,0),method = "ML")
arimaHarga15 <- arima(Hargafinal,order = c(11,1,0),method = "ML")

coeftest(arimaHarga1)
coeftest(arimaHarga2)
coeftest(arimaHarga3)
coeftest(arimaHarga4)
coeftest(arimaHarga5)
coeftest(arimaHarga6)
coeftest(arimaHarga7)
coeftest(arimaHarga8)
coeftest(arimaHarga9)
coeftest(arimaHarga10)
coeftest(arimaHarga11)
coeftest(arimaHarga12)
coeftest(arimaHarga13)
coeftest(arimaHarga14)
coeftest(arimaHarga15)

checkresiduals(arimaHarga1)
checkresiduals(arimaHarga3)

ksnormTest(((residuals(arimaHarga1))))
ksnormTest(((residuals(arimaHarga3))))

ksnormTest(acos(sqrt(residuals(arimaHarga1))))
ksnormTest(acos(sqrt(residuals(arimaHarga3))))

arimaHarga1
arimaHarga3

prediksi_Harga <- forecast(Hargats,model=arimaHarga1,h=90)
plot(prediksi_Harga)
