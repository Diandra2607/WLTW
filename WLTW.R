Harga <- head(prices,251)
Harga_Tanggal <- ymd(Harga$date)
Hargadf <- (data.frame(Harga_Tanggal,Harga$close))
Hargats <- xts(Harga$close,Harga_Tanggal)
summary(Hargats)

plot_Harga <- ggplot(Harga, aes(x= date, y = close)) + geom_line(col="red")
plot_Harga
ggplotly(plot_Harga)

lambda <- BoxCox.lambda(Hargats)
HargaBoxCox <- BoxCox(Hargats,lambda)
adf.test(HargaBoxCox)
Hargafinal <- na.omit(diff(HargaBoxCox),1)
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

checkresiduals(arimaHarga3)
checkresiduals(arimaHarga6)
checkresiduals(arimaHarga7)
checkresiduals(arimaHarga8)
checkresiduals(arimaHarga9)
checkresiduals(arimaHarga10)
checkresiduals(arimaHarga11)
checkresiduals(arimaHarga13)

ksnormTest(((residuals(arimaHarga11))))
ksnormTest(((residuals(arimaHarga13))))

ksnormTest(acos(sqrt(residuals(arimaHarga11))))
ksnormTest(acos(sqrt(residuals(arimaHarga13))))

arimaHarga11
arimaHarga13

prediksi_Harga <- forecast(Hargats,model=arimaHarga11,h=365)
plot(prediksi_Harga)

summary(prediksi_Harga)