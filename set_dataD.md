dixi_data_set <- отдельный (data = dixi_data_set, 
                                  col = дата, 
                                  into = c («дата», «время»), 
                                  sep = "\\")
dixi_data_set $ date <- as.Date (gsub ('(\\ d {2}). (\\ d {2}). (\\ d {4})', '\\ 3 - \\ 2- \ \ 1' , 
                                   dixi_data_set $ date)) # Меняем формат даты обращения
dixi_data_set $ dob <- gsub ('(\\ d {2}). (\\ d {2}). (\\ d {4})', '\\ 3 - \\ 2 - \\ 1', 
                                      dixi_data_set $ dob) # Меняем формат даты рождения
all_leads $ date <- as.Date (gsub ('(\\ d {2}). (\\ d {2}). (\\ d {4})', '\\ 3 - \\ 2- \ \ 1' , 
                               all_leads $ date)) # Меняем формат даты обращения
all_leads $ dob <- gsub ('(\\ d {2}). (\\ d {2}). (\\ d {4})', '\\ 3 - \\ 2 - \\ 1', 
                                  all_leads $ dob) # Меняем формат даты рождения

dixi_data_set <- dixi_data_set [, -c (2, 3, 16)]
all_leads <- all_leads [, -2]

dixi_data_set <- rbind (all_leads, dixi_data_set, deparse.level = 0, make.row.names = F)


# Меняем формат даты обращения
dixi_data_set $ Age <- этаж (difftime (Sys.Date (),
                                    as.Date (dixi_data_set $ д.р.), 
                                    единицы = 'дни') / 365) # Считаем кол-во дней возраста
dixi_data_set <- dixi_data_set [duplicated (dixi_data_set $ Phone) == FALSE,] # Удаляем дубликаты
dixi_data_set $ grad [is.na (dixi_data_set $ grad)] <- 'Россия'
deleteRow <- функция (набор данных, шаблон) {
  finalData <- data.frame ()
  для (я в 1: Nrow (набор данных)) {
    if (! TRUE% в% grepl (шаблон, набор данных [i, 1: ncol (набор данных)])) {
      finalData <- rbind (finalData, набор данных [i,])
    }
  }
  вернуться (FinalData)
}
dixi_data_set <- deleteRow (dixi_data_set, '(Другое | Армения | Узбекистан | Таджикистан | Украина)')
dixi_data_set <- dixi_data_set [-10752,] # Удаляем
dixi_data_set <- подмножество (dixi_data_set, date> = date_start)
dixi_data_set <- подмножество (dixi_data_set, date <= date_end)
dixi_data_set <- подмножество (dixi_data_set, Age> = 18) # Фильтруем
dixi_data_set $ приводит <- 1

# Группировка
dixi_data_set <- dixi_data_set%>%
  group_by (источник, кампания, контент, срок)%>% # группируем таблицу по полю 
  суммировать (ведет = сумма (ведет), # определяет количество Лидов
            phone_unique = длина (уникальный (телефон)))%>% # определите, что к-во 
  организовать (-leads)
# Разбить строки
dixi_data_set <- отдельный (data = dixi_data_set, 
                          col = Кампания, 
                          into = c ("Campaign", "CampaignID"), 
                          sep = "\\ |")
dixi_data_set <- отдельный (data = dixi_data_set, 
                          col = содержание, 
                          into = c ("vakansy", "Ad_ID", "Ban_id", "addphr", "addphr2", "camp_t", "creative_id", "d_t", "Group_ID", "phr_id", "Other"), 
                          sep = "\\ |")
dixi_data_set <- подмножество (dixi_data_set, Source == "yandex")
dixi_data_set <- dixi_data_set [, -c (4:11, 14)]

dixi_data_set <- отдельный (data = dixi_data_set, 
                          col = Group_ID, 
                          into = c ("Group", "Group_ID"), 
                          sep = "\\:")
dixi_data_set <- отдельный (data = dixi_data_set, 
                          col = phr_id, 
                          into = c ("phr", "CriterionId"), 
                          sep = "\\:")
dixi_data_set <- dixi_data_set [, -c (4, 6)]

yandex_stats <- yadirGetReport (ReportType = "CUSTOM_REPORT", 
                               DateRangeType = "CUSTOM_DATE", 
                               DateFrom = date_start, 
                               DateTo = date_end, 
                               FieldNames = c ("CampaignName",
                                              "CampaignId",
                                              "AdGroupName",
                                              "AdGroupId",
                                              «Критерий»,
                                              "CriterionId",
                                              «Впечатление»,
                                              «щелкает»,
                                              "Стоимость"),
                               Логин = "дикси-бетапресс",
                               AgencyAccount = "betapress.direct",
                               TokenPath = "agency_login")

yandex_stats $ CampGroupKey <- вставить (yandex_stats $ CampaignId, yandex_stats $ AdGroupId, yandex_stats $ CriterionId, sep = "-")
dixi_data_set $ CampGroupKey <- вставить (dixi_data_set $ CampaignID, dixi_data_set $ Group_ID, dixi_data_set $ CriterionId, sep = "-")

# Группировка
yandex_stats <- yandex_stats%>%
  group_by (CampGroupKey, CampaignName, AdGroupName, Criterion, CriterionId)%>% # группируем таблицу по полю 
  суммировать (Стоимость = сумма (Стоимость),
            Показы = сумма (Показы),
            Клики = сумма (кликов))%>% 
  организовать (-Стоимость)

dixi_data_set <- dixi_data_set%>%
  group_by (CampGroupKey, CampaignID, Group_ID, CriterionId, Term)%>% # группируем таблицу по полю 
  суммировать (ведет = сумма (ведет) # определите количество Лидов
  )%>% # обратите внимание, что к-во 
  организовать (-leads)


dixi_data_set <- dixi_data_set # приводим к единому значению ретаргет
для (я в 1: nrow (dixi_data_set)) {
  if (grepl ('45533715 | 45533718', as.character (dixi_data_set [i, 1]))) {
    Значение <- as.character (dixi_data_set [i, 1])
    dixi_data_set [i, 5] <- значение
  }
}

dixi_data_set <- dixi_data_set%>%
  group_by (CampGroupKey, CampaignID, Group_ID, CriterionId, Term)%>% # группируем таблицу по полю 
  суммировать (ведет = сумма (ведет) # определите количество Лидов
  )%>% # обратите внимание, что к-во 
  организовать (-leads)

# Авторизация
r.for.rga <- authorize (client.id = "906459915473-4vechj2uecoq79i8ar4o8la1tu963pvm.apps.googleusercontent.com", client.secret = "NTdES2UJL7YSVf6rsCiQtQbE")
# Core API
dixi <- RGA :: get_ga (profileId = "ga: 185981495",
                    start.date = date_start,
                    end.date = date_end,
                    metrics = "ga: пользователи, ga: сессии, ga: goal1Completions",
                    размеры = "ga: дата, ga: sourceMedium, ga: средний, ga: кампания, ga: adContent, ga: ключевое слово",
                    fetch.by = "year") # При ga: пользователи и ga: NdayUsers могут быть неверными

dixi_msk <- RGA :: get_ga (profileId = "ga: 186814149",
                        start.date = date_start,
                        end.date = date_end,
                        metrics = "ga: пользователи, ga: сессии, ga: goal1Completions",
                        размеры = "ga: дата, ga: sourceMedium, ga: средний, ga: кампания, ga: adContent, ga: ключевое слово",
                        fetch.by = "year") # При ga: пользователи и ga: NdayUsers могут быть неверными

zumdixi <- RGA :: get_ga (profileId = "ga: 190171370",
                       start.date = date_start,
                       end.date = date_end,
                       metrics = "ga: пользователи, ga: сессии, ga: goal1Completions",
                       размеры = "ga: дата, ga: sourceMedium, ga: средний, ga: кампания, ga: adContent, ga: ключевое слово",
                       fetch.by = "year") # При ga: пользователи и ga: NdayUsers могут быть неверными



dixi <- подмножество (dixi, sourceMedium == "yandex / cpc") #
dixi_msk <- подмножество (dixi_msk, sourceMedium == "yandex / cpc") #
zumdixi <- подмножество (zumdixi, sourceMedium == "yandex / cpc") #

dixi_all <- rbind (dixi, dixi_msk, zumdixi, deparse.level = 0, make.row.names = F)



dixi_all <- dixi_all%>%
  group_by (кампания, adContent, ключевое слово)%>%
  суммировать (пользователи = сумма (пользователи),
            goal1Completions = сумма (goal1Completions)
  )%>% упорядочить (-goal1Completions)



dixi_all <- отдельный (data = dixi_all, 
                     col = кампания, 
                     into = c ("campaign", "campaign_ID"), 
                     sep = "\\ |")

dixi_all <- отдельный (data = dixi_all, 
                     col = adContent, 
                     into = c ("vakancy", "Adid", "banid", "1", "2", "3", "4", "d_t", "group_id", "criterionid"), 
                     sep = "\\ |")


dixi_all <- dixi_all [, -c (3:10)]


dixi_all <- отдельный (data = dixi_all, 
                     col = group_id, 
                     into = c ("Group", "Group_id"), 
                     sep = "\\:")

dixi_all <- отдельный (data = dixi_all, 
                     col = критерий, 
                     into = c ("Критерий", "Критерион"), 
                     sep = "\\:")


dixi_all <- dixi_all [, -c (3, 5)]

dixi_all $ CampGroupKey <- вставить (dixi_all $ campaign_ID, dixi_all $ Group_id, dixi_all $ Criterionid, sep = "-")


dixi_all <- dixi_all%>%
  group_by (CampGroupKey, campaign_ID, Group_id, Criterionid)%>% # группируем таблицу по полю 
  суммировать (пользователи = сумма (пользователи),
            goal1Completions = сумма (goal1Completions)
  )%>% упорядочить (-goal1Completions)


totalData_dixi <- объединить (yandex_stats, dixi_data_set, by.x = "CampGroupKey", by.y = "CampGroupKey", all.x = TRUE, all.y = T)
totalData_dixi <- объединить (totalData_dixi, dixi_all, by.x = "CampGroupKey", by.y = "CampGroupKey", all.x = TRUE, all.y = T)
totalData_dixi <- totalData_dixi [, -c (5, 9, 10, 11, 12, 14, 15, 16)]
totalData_dixi <- подмножество (totalData_dixi, показы> = 1)
totalData_dixi $ CPL_rel <- totalData_dixi $ Cost / totalData_dixi $ ведет
totalData_dixi $ CPL_base <- totalData_dixi $ Стоимость / totalData_dixi $ goal1Completions
totalData_dixi [is.na (totalData_dixi)] <- ''
totalData_dixi $ CPL_base [totalData_dixi $ CPL_base == "Inf"] <- "0"

удалить (dixi, dixi_msk, zumdixi, all_leads, dixi_all, yandex_stats, title_dixi, title_old, dixi_data_set)
