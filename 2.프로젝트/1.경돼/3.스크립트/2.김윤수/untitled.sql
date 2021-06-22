-- rename columns
CREATE OR REPLACE TABLE `utopian-surface-268707.YS_DATASET.CHAWCHAWTIME`
AS
SELECT
  ______ AS date1,
  _______1 AS day_of_week,
  _______________ AS workforce,
  __________________ AS num_vacation,
  ___________________4 AS num_businesstrip,
  __________________________________________ AS num_overtime_work,
  _________________________________ AS num_athome_worker,
  ____________ AS breakfast,
  _____________8 AS lunch,
  _____________9 AS dinner,
  _________ AS count_lunch,
  __________11 AS count_dinner
FROM
  `utopian-surface-268707.YS_DATASET.CHAWCHAWTIME`

-- count days to see the imbalance
WITH A AS (
SELECT day_of_week, COUNT(day_of_week) OVER(PARTITION BY day_of_week)
FROM `utopian-surface-268707.YS_DATASET.CHAWCHAWTIME`
) SELECT DISTINCT * FROM A ORDER BY day_of_week DESC


-- checking date rage
SELECT date1 
FROM `utopian-surface-268707.YS_DATASET.CHAWCHAWTIME`
ORDER BY date1 ASC

--Create date columns
CREATE OR REPLACE TABLE
  `utopian-surface-268707.YS_DATASET.CHAWCHAWTIME2` AS
WITH
  A AS (
  SELECT
    *,
    CAST(SUBSTR(CAST(date1 AS STRING), 1, 4) AS INT64) AS year,
    CAST(SUBSTR(CAST(date1 AS STRING), 6, 2) AS INT64) AS month,
    CAST(SUBSTR(CAST(date1 AS STRING), 9, 2) AS INT64) AS day,
    SUBSTR(CAST(date1 AS STRING), 1, 7) AS year_month_str
  FROM
    `utopian-surface-268707.YS_DATASET.CHAWCHAWTIME` )
SELECT
  * EXCEPT(day_of_week),
  CASE day_of_week
    WHEN '월' THEN 1
    WHEN '화' THEN 2
    WHEN '수' THEN 3
    WHEN '목' THEN 4
    WHEN '금' THEN 5
END
  AS day_of_week
FROM
  A
  
