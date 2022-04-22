--посмотрим, какие тарифные планы используют премиум-пользователи
SELECT pu.user_id,
	   p.description
  FROM premium_users AS pu
  JOIN plans AS p
	   ON pu.membership_plan_id = p.id;

--посмотрим названия песен, которые cлушает каждый пользователь
SELECT p.user_id,
	   s.title
  FROM plays AS p
  JOIN songs AS s
	   ON p.song_id = s.id;

--Какие пользователи не являются премиум-пользователями?
SELECT u.id
  FROM users AS u
  LEFT JOIN premium_users AS pu
	   ON u.id = pu.user_id
 WHERE pu.user_id IS NULL;

--С помощью СTE найдите пользователей, которые слушали музыку в январе и феврале, а потом оставьте
--только тех пользователей, которые слушали музыку ТОЛЬКО в январе.
WITH january AS
	(SELECT *
	   FROM plays
	  WHERE EXTRACT(MONTH FROM play_date) = 1),
	
	february AS
	(SELECT *
	   FROM plays
	  WHERE EXTRACT(MONTH FROM play_date) = 2)

SELECT j.user_id
  FROM january AS j
  LEFT JOIN february AS f
	   ON j.user_id = f.user_id 
 WHERE f.user_id IS NULL;

--Для каждого месяца в таблице months мы хотим знать, был ли каждый премиум-пользователь
--активным или удаленным (не продлевал свою подписку на сервис)
SELECT m.months::date,
	   pu.purchase_date::date AS purchase_date,
	   pu.cancel_date::date AS cancel_date,
  FROM months AS m
 CROSS JOIN premium_users AS pu;

 --Определить какие пользователи у нас активные, а какие не активные в каждом месяце
SELECT pu.user_id,
  	   m.months,
  CASE WHEN (pu.purchase_date <= m.months)
       AND (pu.cancel_date >= m.months OR pu.cancel_date IS NULL)
       THEN 'active'
       ELSE 'not_active'
  	   END AS status

  FROM months AS m
 CROSS JOIN premium_users AS pu;

--Объедините таблицу songs и bonus_songs с помощью UNION и выберите все столбцы.
--Поскольку таблица songs очень большая, просто посмотрите на некий срез данных
--и выведите только 10 строк с помощью LIMIT.
SELECT *
  FROM songs
 UNION
SELECT *
  FROM bonus_songs
 LIMIT 10;

--Найти количество раз, которое была прослушана каждая песня,
--добавить дополнительную информацию из таблицs songs
WITH play_count AS
	(SELECT p.song_id,
	        COUNT(*) AS "times_played"
	   FROM plays AS p
	  GROUP BY p.song_id)

SELECT s.title,
	   s.artist,
	   pc.times_played
  FROM play_count AS pc
  JOIN songs AS s
		ON pc.song_id = s.id;