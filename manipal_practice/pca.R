library(dummies)
Test_Big$Item_Outlet_Sales <- 1
Test_Big_combine <- rbind(Train_Big,Test_Big)
Test_Big_combine$Item_Weight[is.na(Test_Big_combine$Item_Weight)] <- median(Test_Big_combine$Item_Weight, na.rm = TRUE)
Test_Big_combine$Item_Visibility <- ifelse(Test_Big_combine$Item_Visibility == 0, median(Test_Big_combine$Item_Visibility),Test_Big_combine$Item_Visibility)

table(Test_Big_combine$Outlet_Size, Test_Big_combine$Outlet_Type)
levels(Test_Big_combine$Outlet_Size)[1] <- "other"

final_data <- subset(Test_Big_combine, select = -c(Item_Identifier, Item_Outlet_Sales, Outlet_Identifier))
colnames(final_data)
new_data <- dummy.data.frame(final_data, names = c("Item_Fat_Content",
                                                   "Item_Type",
                                                   "Outlet_Establishment_Year",
                                                   "Outlet_Size",
                                                   "Outlet_Location_Type",
                                                   "Outlet_Type"))

