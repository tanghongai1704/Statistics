#####################################################
# THƯ VIỆN
#####################################################

# Cài đặt và load các thư viện được sử dụng 
libs <- c("readr", "lmtest", "car", "ggplot2", "dplyr", 
"patchwork","corrplot", "dplyr", "tidyr", "stringr","forcats")

for (pkg in libs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

#####################################################
# 1 - TIEN XU LY DU LIEU
#####################################################

# ===================================================
# 1.1. Doc tap du lieu
# ===================================================

# Doc du lieu
data <- read.csv("D:/Study/HCMUT/Semester/HK242/XSTK/BTL/dataset/StudentsPerformance.csv", 
                 stringsAsFactors = FALSE,
                 check.names = FALSE)  # Chỉnh lại cho đúng đường dẫn ở local

# Thay doi dinh dang ten
names(data) <- gsub(" ", "_", names(data))  # Renaming names for easier access

# ===================================================
# 1.2. Tong ket du lieu
# ===================================================
summary(data)

# ===================================================
# 1.3. Kiem tra du lieu bi thieu
# ===================================================
apply(is.na(data), 2, sum)

# ===================================================
# 1.4. Kiem tra va chuyen doi kieu du lieu
# ===================================================

# Kiem tra kieu du lieu hien tai
sapply(data, class)

# Thay doi kieu du lieu (factor & numeric)
data <- data %>%
  mutate(
    gender                       = factor(gender),
    `race/ethnicity`             = factor(`race/ethnicity`),
    parental_level_of_education = factor(parental_level_of_education),
    lunch                        = factor(lunch),
    test_preparation_course      = factor(test_preparation_course),
    math_score                   = as.numeric(math_score),
    reading_score                = as.numeric(reading_score),
    writing_score                = as.numeric(writing_score)
  )

# ===================================================
# 1.5. Luu du lieu da qua xu ly
# ===================================================
write.csv(data, "D:/Study/HCMUT/Semester/HK242/XSTK/BTL/dataset/StudentsPerformance_Cleaned.csv", row.names = FALSE) # Chỉnh lại cho đúng đường dẫn ở local

#####################################################
# 4 - THỐNG KÊ MÔ TẢ
#####################################################

# ===================================================
# 4.1 - Thống kê dạng bảng
# ===================================================

# ===================================================
# 4.1.1 - Dữ liệu định lượng
# ===================================================

# Tính dữ liệu số
cols = c("math_score", "reading_score", "writing_score")
score = c("Math score", "Reading score", "Writing score")
mean = apply(data[cols], 2, mean, na.rm = TRUE)
std = apply(data[cols], 2, sd, na.rm = TRUE)
min = apply(data[cols], 2, min, na.rm = TRUE)
first_quantitle = apply(data[cols], 2, quantile, na.rm = TRUE, probs = 0.25)
median = apply(data[cols], 2, median, na.rm = TRUE)
third_quantitle = apply(data[cols], 2, quantile, na.rm = TRUE, probs = 0.75)
max = apply(data[cols], 2, max, na.rm = TRUE)

df = data.frame(score, mean, std, min, first_quantitle, median, third_quantitle, max)
rownames(df) = NULL
colnames(df) = c("", "Mean", "Standard deviation", "Min", "First quantitle", "Median", "Third quantitle", "Max")
View(df)

# ===================================================
# 4.1.2 - Dữ liệu định tính
# ===================================================

# Statistics Categorical data  
tbl <- as.data.frame(table(data$`race/ethnicity`))
names(tbl) <- c("race/ethnicity", "Freq")
View(tbl)
View(as.data.frame(table(data$parental_level_of_education, dnn="parental_level_of_education")))
View(as.data.frame(table(data$lunch, dnn="lunch")))
View(as.data.frame(table(data$gender, dnn="gender")))
View(as.data.frame(table(data$test_preparation_course, dnn="test_preparation_course")))


# Tổng kết các đặc tính dữ liệu

cate_rows = c("race/ethnicity", "parental_level_of_education", "lunch", "gender", "test_preparation_course")
summary_cate = data.frame(
  Unique = integer(), 
  Mode = character(), 
  Frequency = integer(), 
  stringsAsFactors = FALSE
)
for (i in cate_rows) {
  vec = data[[i]]                         # Truy cập cột bất kể tên đặc biệt
  uniq = length(unique(vec))             # Số giá trị duy nhất
  mod_table = sort(table(vec), decreasing = TRUE)  # Bảng tần suất giảm dần
  mod = names(mod_table)[1]              # Giá trị mode
  freq = as.integer(mod_table[1])        # Tần suất của mode
  newrow <- data.frame(Unique = uniq, Mode = mod, Frequency = freq)
  summary_cate = rbind(summary_cate, newrow)
}
rownames(summary_cate) = cate_rows
View(summary_cate)

# ===================================================
# 4.2 - Trực quan hóa dữ liệu
# ===================================================

# ===================================================
# 4.2.1 - Dữ liệu định lượng
# ===================================================

# Histogram-Boxplot for Math_Score

par(mfrow = c(1, 2))  # 1 hàng và 2 cột, Đặt cấu hình để vẽ 2 biểu đồ ngang nhau
hist(data[["math_score"]], breaks = seq(0, 100, by = 5),
     xlab = "Math score", ylab = "Frequency", main = "Histogram of Math Scores",
     col = "lightblue")
boxplot(data[["math_score"]], ylab = "Frequency", main = "Boxplot of Math Scores",
        col = "darkred")  # Vẽ biểu đồ boxplot
par(mfrow = c(1, 1))  # Quay lại cấu hình mặc định của đồ thị (1 đồ thị)

# Histogram-Boxplot for Reading_Score

par(mfrow = c(1, 2))  # 1 hàng và 2 cột
hist(data[["reading_score"]],  breaks = seq(0, 100, by = 5), 
     xlab = "Reading score", ylab = "Frequency", main = "Histogram of Reading Scores", 
     col ="lightgreen")
boxplot(data[["reading_score"]], ylab = "Frequency", main = "Boxplot of Reading Scores", 
        col ="purple")
par(mfrow = c(1, 1)) # Đặt lại về 1 hàng 1 cột

# Histogram-Boxplot for Writing_Score

par(mfrow = c(1, 2))  # 1 hàng và 2 cột
hist(data[["writing_score"]],  breaks = seq(0, 100, by = 5), 
     xlab = "Writing score", ylab = "Frequency", main = "Histogram of Writing Scores", 
     col = "#f07a20")
boxplot(data[["writing_score"]], ylab = "Frequency", main = "Boxplot of Writing scores", 
        col = "#f02084")
par(mfrow = c(1, 1))

# ===================================================
# 4.2.2 - So sánh định tính và định lượng
# ===================================================

# Boxplot for gender

Math_Gender <- ggplot(data, 
                      aes(x = gender, y = math_score)) +
  geom_boxplot(fill = c("lightgreen", "lemonchiffon1"), 
               color = "black") +
  labs(title = "Math Score vs Gender",
       x = "Gender", 
       y = "Math Score (points)") +
  ylim(0, 100)
Reading_Gender <- ggplot(data, 
                         aes(x = gender, y = reading_score)) +
  geom_boxplot(fill = c("lightgreen", "lemonchiffon1"), 
               color = "black") +
  labs(title = "Reading Score vs Gender",
       x = "Gender", 
       y = "Reading Score (points)") +
  ylim(0, 100)
Writing_Gender <- ggplot(data, 
                         aes(x = gender, y = writing_score)) +
  geom_boxplot(fill = c("lightgreen", "lemonchiffon1"), 
               color = "black") +
  labs(title = "Writing Score vs Gender",
       x = "Gender", 
       y = "Writing Score (points)") +
  ylim(0, 100)
Math_Gender + Reading_Gender + Writing_Gender

# Boxplot for Race/Ethnicity

Math_Race <- ggplot(data, 
                    aes(x = `race/ethnicity`, y = math_score, fill = `race/ethnicity`)) +
  geom_boxplot(color = "black") +
  labs(title = "Math Score vs Race/Ethnicity",
       x = "Race/Ethnicity", y = "Math Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position = "none")

Reading_Race <- ggplot(data, 
                       aes(x = `race/ethnicity`, y = reading_score, fill = `race/ethnicity`)) +
  geom_boxplot(color = "black") +
  labs(title = "Reading Score vs Race/Ethnicity",
       x = "Race/Ethnicity", y = "Reading Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position = "none")

Writing_Race <- ggplot(data, 
                       aes(x = `race/ethnicity`, y = writing_score, fill = `race/ethnicity`)) +
  geom_boxplot(color = "black") +
  labs(title = "Writing Score vs Race/Ethnicity",
       x = "Race/Ethnicity", y = "Writing Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position = "none")

Math_Race + Reading_Race + Writing_Race

# Boxplot for Parental Level of Education 

Math_Parental <- ggplot(data, 
                       aes(x = parental_level_of_education, y = math_score, fill = parental_level_of_education)) +
  geom_boxplot(color = "black") +
  labs(title = "Math Score vs Parental Level of Education",
       x = "Parental Level of Education", y = "Math Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
Reading_Parental <- ggplot(data, 
                           aes(x = parental_level_of_education, y = reading_score, fill = parental_level_of_education)) +
  geom_boxplot(color = "black") +
  labs(title = "Reading Score vs Parental Level of Education",
       x = "Parental Level of Education", y = "Reading Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

Writing_Parental <- ggplot(data,
                           aes(x = parental_level_of_education, y = writing_score, fill = parental_level_of_education)) +
  geom_boxplot(color = "black") +
  labs(title = "Writing Score vs Parental Level of Education",
       x = "Parental Level of Education", y = "Writing Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
Math_Parental + Reading_Parental + Writing_Parental

# Boxplot for Lunch

Math_Lunch <- ggplot(data, 
                     aes(x = lunch, y = math_score, fill = lunch)) +
  geom_boxplot(color = "black") +
  labs(title = "Math Score vs Lunch",
       x = "Lunch Type", y = "Math Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position = "none")
Reading_Lunch <- ggplot(data, 
                        aes(x = lunch, y = reading_score, fill = lunch)) +
  geom_boxplot(color = "black") +
  labs(title = "Reading Score vs Lunch",
       x = "Lunch Type", y = "Reading Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position = "none")
Writing_Lunch <- ggplot(data, 
                        aes(x = lunch, y = writing_score, fill = lunch)) +
  geom_boxplot(color = "black") +
  labs(title = "Writing Score vs Lunch",
       x = "Lunch Type", y = "Writing Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position = "none")
Math_Lunch + Reading_Lunch + Writing_Lunch

# Boxplot for Test Preparation

Math_TestPrep <- ggplot(data, 
                        aes(x = test_preparation_course, y = math_score, fill = test_preparation_course)) +
  geom_boxplot(color = "black") +
  labs(title = "Math Score vs Test Preparation",
       x = "Test Preparation Course", y = "Math Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position = "none")
Reading_TestPrep <- ggplot(data, 
                           aes(x = test_preparation_course, y = reading_score, fill = test_preparation_course)) +
  geom_boxplot(color = "black") +
  labs(title = "Reading Score vs Test Preparation",
       x = "Test Preparation Course", y = "Reading Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position = "none")
Writing_TestPrep <- ggplot(data, 
                           aes(x = test_preparation_course, y = writing_score, fill = test_preparation_course)) +
  geom_boxplot(color = "black") +
  labs(title = "Writing Score vs Test Preparation",
       x = "Test Preparation Course", y = "Writing Score (points)") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(legend.position = "none")
Math_TestPrep + Reading_TestPrep + Writing_TestPrep

# ===================================================
# 4.2.3 - Tương quan giữa dữ liệu định lượng
# ===================================================

## Scatterplots for Math-Reading-Writing
# library(ggplot2)

# Math vs Reading
Math_vs_Reading <- ggplot(data,
                          aes(x = reading_score, y = math_score, color = reading_score)) +
  geom_point(position = position_jitter(width = 0.5, height = 0.5), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(title = "Math Score vs Reading Score",
       x = "Reading Score",
       y = "Math Score") +
  scale_color_gradient(low = "royalblue", high = "#05008b") +
  xlim(0, 100) + ylim(0, 100) +
  theme_minimal() +
  guides(color = "none")  # Ẩn legend

# Math vs Writing

Math_vs_Writing <- ggplot(data,
                          aes(x = writing_score, y = math_score, color = writing_score)) +
  geom_point(position = position_jitter(width = 0.5, height = 0.5), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Math Score vs Writing Score",
       x = "Writing Score",
       y = "Math Score") +
  scale_color_gradient(low = "tomato", high = "red") +
  xlim(0, 100) + ylim(0, 100) +
  theme_minimal() +
  guides(color = "none")

# Reading vs Writing

Writing_vs_Reading <- ggplot(data,
                             aes(x = reading_score, y = writing_score, color = reading_score)) +
  geom_point(position = position_jitter(width = 0.5, height = 0.5), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "orange4") +
  labs(title = "Writing Score vs Reading Score",
       x = "Reading Score",
       y = "Writing Score") +
  scale_color_gradient(low = "lightseagreen", high = "darkgreen") +
  xlim(0, 100) + ylim(0, 100) +
  theme_minimal() +
  guides(color = "none")
Math_vs_Reading + Math_vs_Writing + Writing_vs_Reading

## Tương quan giữa các dữ liệu số
# library(corrplot)

# Chỉ chọn 3 cột liên quan
selected_data <- data[, c("math_score", "reading_score", "writing_score")]
# Tính ma trận tương quan
cor_matrix <- cor(selected_data)
# Vẽ biểu đồ tương quan
corrplot(cor_matrix,
         method = "circle",
         type = "full",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.cex = 1,
         tl.srt = 30,
         addCoef.col = "black",
         number.cex = 1,
         mar = c(1, 1, 1, 1),
         diag = TRUE)

#####################################################
# 5 - THONG KE SUY DIEN
#####################################################

# ===================================================
# 5.1. Khoang tin cay
# ===================================================

# Kiem dinh gia dinh Phan phoi chuan
shapiro.test(data$writing_score)

# Cac dac trung du lieu
n<-length(data$writing_score)
xtb<-mean(data$writing_score)
sd <-sd(data$writing_score)
data.frame(n,xtb,sd)

# Sai so
Epsilon <- qnorm(p =0.05/2,lower.tail=FALSE)*sd/sqrt(n)
print(Epsilon)

# Khoang tin cay
left<-xtb-Epsilon
right<-xtb+Epsilon
data.frame(left,right)

# ===================================================
# 5.2. ANOVA 1 nhan to
# ===================================================

# Kiem dinh gia dinh Phan phoi chuan cua tung phan khuc
by(data$writing_score, data$parental_level_of_education, shapiro.test)

# Kiem dinh gia dinh Phuong sai dong nhat
leveneTest(writing_score ~ parental_level_of_education, data = data)

# Ket qua ANOVA
anova_result<-aov(writing_score ~ parental_level_of_education, data=data)
summary(anova_result)

# ===================================================
# 5.3. Mô hình hồi quy tuyến tính
# ===================================================

# ===================================================
# 5.3.1. Tiền dữ liệu của mô hình
# ===================================================

# Đưa các biến phân loại thành dummy variables
data$gender <- as.factor(data$gender)
data$`race/ethnicity` <- as.factor(data$`race/ethnicity`)
data$parental_level_of_education <- as.factor(data$parental_level_of_education)
data$test_preparation_course <- as.factor(data$test_preparation_course)
data$lunch <- as.factor(data$lunch)

# Chia dữ liệu thành tập huấn luyện và tập kiểm tra (80% train, 20% test)
set.seed(123) # Không thay đổi sau mỗi lần chạy lại

train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# ===================================================
# 5.3.2. Mô hình hồi quy tuyến tính đa biến
# ===================================================

model <- lm(writing_score ~ gender + `race/ethnicity` + parental_level_of_education + lunch + test_preparation_course + math_score + reading_score, data = train_data)
summary(model)

# Stepwise regression
model_stepwise <- step(model, direction = "both", trace = 1)

# ===================================================
# 5.3.3. Kiểm định
# ===================================================

## Kiểm định phân phối của phần dư

shapiro.test(model_stepwise$residuals)

## Kiểm định phương sai đồng nhất

# library(lmtest)
bptest(model_stepwise)

plot(model_stepwise$fitted.values, model_stepwise$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

## Kiểm định phần dư độc lập (không có sự tương quan)

# library(lmtest)
dwtest(model_stepwise)

## Kiểm định đa cộng tuyến

# library(car)
vif(model_stepwise)

# ===================================================
# 5.3.3. Hệ số của mô hình hồi quy tuyến tính
# ===================================================

coefficients(model_stepwise)

# ===================================================
# 5.3.4. Kiểm định
# ===================================================

# Dự đoán mô hình trong tập test
predictions <- predict(model_stepwise, newdata = test_data)

# So sánh giá trị thực tế và giá trị dự đoán ở các index ngẫu nhiên
set.seed(123)

random_indices <- sample(1:nrow(test_data), 10)
sorted_indices <- sort(random_indices)
data.frame(
  Index = sorted_indices,
  Actual = test_data$writing_score[sorted_indices],
  Predicted = predictions[sorted_indices]
)

# Vẽ biểu dự đoán và giá trị thực tế
# library(ggplot2)

df_plot <- data.frame(Actual = test_data$writing_score, Predicted = predictions)

ggplot(df_plot, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 1) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) + 
  labs(title = "Actual vs Predicted",
       x = "Actual", 
       y = "Predicted") + 
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Đánh giá mô hình trên tập test
mse <- mean((predictions - test_data$writing_score)^2)
rmse <- sqrt(mse)
ss_total <- sum((test_data$writing_score - mean(test_data$writing_score))^2)
ss_residual <- sum((test_data$writing_score - predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)
cat(" Mean Squared Error (MSE):", mse, "\n", "Root Mean Squared Error (RMSE):", rmse, "\n", "R-squared:", r_squared, "\n")
