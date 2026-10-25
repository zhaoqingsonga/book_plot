-- F1 群体试验：把「母本」「父本」占位行换成真实品种名
--
-- 用法：sqlite3 data/field_book.sqlite < scripts/fix_f1_parent_names.sql
--
-- 原理：
--   群体 F1 试验的田间记录按 code 连续排布，每组三行：
--     code = C   组合行（3 行区），name = 组合代号，ma/pa = 母本名/父本名
--     code = C+1 母本行（1 行区），name 原本只写 '母本'
--     code = C+2 父本行（1 行区），name 原本只写 '父本'
--   因此母本行的名称取自组合行的 ma，父本行取自 pa。
--   ma/pa 原样保留，不做任何规范化（保留 '大明白豆子<1>'、'22QZY141' 这类写法）。
--
-- 幂等：只处理 name 仍为 '母本'/'父本' 的行，重复执行不会二次改写。
-- 涉及试验：
--   N-3.1-常规-群体F1-宿州20260529        （432 组合 → 864 行）
--   G-3.1-转基因-群体F1-宿州-20260426      （52 组合 → 104 行）
-- 回滚：见 scripts/rollback_f1_parent_names.sql

BEGIN IMMEDIATE;

CREATE TEMP TABLE f1_parent_rename_map AS
SELECT t.record_id                                   AS record_id,
       t.experiment_name                             AS experiment_name,
       t.name                                        AS old_name,
       CASE WHEN t.name = '母本' THEN c.ma ELSE c.pa END AS new_name
FROM   population_field_records t
JOIN   population_field_records c
       ON  c.experiment_name = t.experiment_name
       AND c.code = t.code - CASE WHEN t.name = '母本' THEN 1 ELSE 2 END
WHERE  t.experiment_name IN ('N-3.1-常规-群体F1-宿州20260529',
                             'G-3.1-转基因-群体F1-宿州-20260426')
  AND  t.name IN ('母本', '父本')
  AND  c.name NOT IN ('母本', '父本')
  AND  c.ma IS NOT NULL AND TRIM(c.ma) <> ''
  AND  c.pa IS NOT NULL AND TRIM(c.pa) <> '';

UPDATE population_field_records
SET    name = (SELECT m.new_name FROM f1_parent_rename_map m
               WHERE m.record_id = population_field_records.record_id)
WHERE  record_id IN (SELECT record_id FROM f1_parent_rename_map);

COMMIT;
