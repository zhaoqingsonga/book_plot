PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
DROP TABLE IF EXISTS population_records;
CREATE TABLE population_records (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL UNIQUE,
      experiment_name TEXT NOT NULL,
      total_rows REAL DEFAULT 0,
      has_generated INTEGER DEFAULT 0,
      raw_data TEXT,
      generated_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    );
DROP TABLE IF EXISTS line_selection_records;
CREATE TABLE line_selection_records (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL UNIQUE,
      source_id TEXT,
      experiment_name TEXT NOT NULL,
      total_rows REAL DEFAULT 0,
      has_generated INTEGER DEFAULT 0,
      raw_data TEXT,
      generated_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    );
DROP TABLE IF EXISTS yield_test_records;
CREATE TABLE yield_test_records (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL UNIQUE,
      experiment_name TEXT NOT NULL,
      total_rows REAL DEFAULT 0,
      has_generated INTEGER DEFAULT 0,
      raw_data TEXT,
      generated_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    );
DROP TABLE IF EXISTS population_materials;
CREATE TABLE population_materials (
      material_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      fieldid TEXT,
      code TEXT,
      ma TEXT,
      pa TEXT,
      f INTEGER,
      stageid TEXT,
      name TEXT,
      rows REAL DEFAULT 0,
      line_number TEXT,
      rp INTEGER DEFAULT 1,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES population_records(experiment_id) ON DELETE CASCADE
    );
DROP TABLE IF EXISTS line_selection_materials;
CREATE TABLE line_selection_materials (
      material_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      fieldid TEXT,
      code TEXT,
      ma TEXT,
      pa TEXT,
      stageid TEXT,
      name TEXT,
      rows REAL DEFAULT 0,
      line_number TEXT,
      rp INTEGER DEFAULT 1,
      sele REAL DEFAULT 0,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES line_selection_records(experiment_id) ON DELETE CASCADE
    );
DROP TABLE IF EXISTS yield_test_materials;
CREATE TABLE yield_test_materials (
      material_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      fieldid TEXT,
      code TEXT,
      ma TEXT,
      pa TEXT,
      stageid TEXT,
      name TEXT,
      rows REAL DEFAULT 0,
      line_number TEXT,
      rp INTEGER DEFAULT 1,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES yield_test_records(experiment_id) ON DELETE CASCADE
    );
DROP TABLE IF EXISTS unified_records;
CREATE TABLE unified_records (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL UNIQUE,
      experiment_type TEXT NOT NULL CHECK(experiment_type IN ('population', 'line_selection', 'yield_test')),
      experiment_name TEXT NOT NULL,
      source_id TEXT,
      total_rows REAL DEFAULT 0,
      has_generated INTEGER DEFAULT 0,
      generated_at TEXT,
      location TEXT,
      prefix TEXT,
      interval_n INTEGER,
      rp INTEGER DEFAULT 1,
      digits INTEGER DEFAULT 3,
      rows_n INTEGER DEFAULT 2,
      ck TEXT,
      min_f INTEGER,
      max_f INTEGER,
      rows_col TEXT,
      seeds_col TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    );
DROP TABLE IF EXISTS unified_materials;
CREATE TABLE unified_materials (
      material_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      experiment_type TEXT NOT NULL,
      fieldid TEXT,
      code TEXT,
      ma TEXT,
      pa TEXT,
      mapa TEXT,
      f INTEGER,
      stageid TEXT,
      name TEXT,
      rows REAL DEFAULT 0,
      line_number TEXT,
      rp INTEGER DEFAULT 1,
      sele REAL DEFAULT 0,
      seeds REAL DEFAULT 0,
      place TEXT,
      next_stage TEXT,
      process TEXT,
      path TEXT,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES unified_records(experiment_id) ON DELETE CASCADE
    );
DROP TABLE IF EXISTS traits_survey;
CREATE TABLE traits_survey (
      survey_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      material_id INTEGER,
      fieldid TEXT,
      code TEXT,
      stageid TEXT,
      name TEXT,
      survey_date TEXT,
      sowing_date TEXT,
      emergence_date TEXT,
      flowering_date TEXT,
      maturity_date TEXT,
      plant_height REAL,
      bottom_pod_height REAL,
      main_stem_nodes INTEGER,
      branches INTEGER,
      leaf_shape TEXT,
      flower_color TEXT,
      pod_setting TEXT,
      lodging TEXT,
      leaf_drop TEXT,
      disease_resistance TEXT,
      pest_resistance TEXT,
      seed_color TEXT,
      seed_size TEXT,
      seed_luster TEXT,
      protein_content REAL,
      oil_content REAL,
      harvest_rows INTEGER,
      harvest_weight REAL,
      seeds_weight REAL,
      notes TEXT,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES unified_records(experiment_id) ON DELETE CASCADE
    );
DROP TABLE IF EXISTS population_field_records;
CREATE TABLE population_field_records (record_id INTEGER PRIMARY KEY AUTOINCREMENT,experiment_id TEXT NOT NULL,experiment_name TEXT NOT NULL,fieldid TEXT,id TEXT,user TEXT,stageid TEXT,name TEXT,ma TEXT,pa TEXT,mapa TEXT,memo TEXT,stage TEXT,next_stage TEXT,f TEXT,sele REAL,process TEXT,path TEXT,source TEXT,former_fieldid TEXT,former_stageid TEXT,code TEXT,rp INTEGER,treatment TEXT,place TEXT,rows REAL,line_number TEXT,is_ck TEXT,XiaoQuShiShouMianJi REAL,XiaoQuChanLiang REAL,HanShuiLiang REAL,MuChan REAL,BoZhongQi TEXT,ChuMiaoQi TEXT,ChuMiaoLiangFou TEXT,MiaoQiTianJianPingJia TEXT,KaiHuaQi TEXT,HuaSe TEXT,HuaQiTianJianPingJia TEXT,YeXing TEXT,RongMaoSe TEXT,ShengZhangXiXing TEXT,JieJiaXiXing TEXT,DaoFuXing TEXT,ZaoShuaiXing TEXT,ZhuXing TEXT,LuoYeXing TEXT,LieJiaXing TEXT,ChengShuQi TEXT,HuoGanChengShu TEXT,ChengShuQiTianJianPingJia TEXT,ShouHuoQi TEXT,XiaoQuShouHuoZhuShu INTEGER,ShengYuQi INTEGER,TianJianBeiZhu TEXT,HuaYeBingDuBing TEXT,NiJingDianZhongFuBing TEXT,ShuangMeiBing TEXT,HuiBanBing TEXT,XiJunXingBanDianBing TEXT,XiuBing TEXT,GenFuBing TEXT,BaoNangXianChongBing TEXT,QiTaBingHai TEXT,DouGanHeiQianYing TEXT,DouJiaMing TEXT,YaChong TEXT,ShiYeXingHaiChong TEXT,KaoZhongZhuShu INTEGER,ZhuGao REAL,DiJiaGao REAL,FenZhiShu INTEGER,ZhuJingJieShu INTEGER,JiaXing TEXT,JiaShuSe TEXT,YouXiaoJia INTEGER,WuXiaoJia INTEGER,DanZhuJiaShu INTEGER,DanZhuLiShu INTEGER,DanZhuLiZhong REAL,MeiJiaLiShu REAL,LiXing TEXT,ZhongPiSe TEXT,QiSe TEXT,ZiYeSe TEXT,ZhongPiGuangZe TEXT,BaiLiZhong REAL,WanHaoLiLv REAL,PoSuiLiLv REAL,BingLiLv REAL,ZiBanLiLv REAL,HeBanLiLv REAL,ShuangMeiLiLv REAL,HuiBanLiLv REAL,ChongShiLiLv REAL,ZiLiPingJia TEXT,DanBai REAL,ZhiFang REAL,DanZhiHe REAL,CaoGanLinKangXing TEXT,ShiZhiJianCe TEXT,HanJiYin TEXT,BoZhongPenShu INTEGER,BoZhongLiShu INTEGER,ChuMiaoShu INTEGER,ChuMiaoLiShu REAL,NaiYanXing TEXT,NaiHanXing TEXT,ShiHuaQi TEXT,ZaJiaoHuaShu INTEGER,ChengHuoJiaShu INTEGER,ZhaJiaoliShu INTEGER,ChuShuQi TEXT,WanShuQi TEXT,HuiFuLv REAL,SSRBuHeGeWeiDian TEXT,created_at TEXT NOT NULL);
DROP TABLE IF EXISTS line_selection_field_records;
CREATE TABLE line_selection_field_records (record_id INTEGER PRIMARY KEY AUTOINCREMENT,experiment_id TEXT NOT NULL,experiment_name TEXT NOT NULL,fieldid TEXT,id TEXT,user TEXT,stageid TEXT,name TEXT,ma TEXT,pa TEXT,mapa TEXT,memo TEXT,stage TEXT,next_stage TEXT,f TEXT,sele REAL,process TEXT,path TEXT,source TEXT,former_fieldid TEXT,former_stageid TEXT,code TEXT,rp INTEGER,treatment TEXT,place TEXT,rows REAL,line_number TEXT,is_ck TEXT,XiaoQuShiShouMianJi REAL,XiaoQuChanLiang REAL,HanShuiLiang REAL,MuChan REAL,BoZhongQi TEXT,ChuMiaoQi TEXT,ChuMiaoLiangFou TEXT,MiaoQiTianJianPingJia TEXT,KaiHuaQi TEXT,HuaSe TEXT,HuaQiTianJianPingJia TEXT,YeXing TEXT,RongMaoSe TEXT,ShengZhangXiXing TEXT,JieJiaXiXing TEXT,DaoFuXing TEXT,ZaoShuaiXing TEXT,ZhuXing TEXT,LuoYeXing TEXT,LieJiaXing TEXT,ChengShuQi TEXT,HuoGanChengShu TEXT,ChengShuQiTianJianPingJia TEXT,ShouHuoQi TEXT,XiaoQuShouHuoZhuShu INTEGER,ShengYuQi INTEGER,TianJianBeiZhu TEXT,HuaYeBingDuBing TEXT,NiJingDianZhongFuBing TEXT,ShuangMeiBing TEXT,HuiBanBing TEXT,XiJunXingBanDianBing TEXT,XiuBing TEXT,GenFuBing TEXT,BaoNangXianChongBing TEXT,QiTaBingHai TEXT,DouGanHeiQianYing TEXT,DouJiaMing TEXT,YaChong TEXT,ShiYeXingHaiChong TEXT,KaoZhongZhuShu INTEGER,ZhuGao REAL,DiJiaGao REAL,FenZhiShu INTEGER,ZhuJingJieShu INTEGER,JiaXing TEXT,JiaShuSe TEXT,YouXiaoJia INTEGER,WuXiaoJia INTEGER,DanZhuJiaShu INTEGER,DanZhuLiShu INTEGER,DanZhuLiZhong REAL,MeiJiaLiShu REAL,LiXing TEXT,ZhongPiSe TEXT,QiSe TEXT,ZiYeSe TEXT,ZhongPiGuangZe TEXT,BaiLiZhong REAL,WanHaoLiLv REAL,PoSuiLiLv REAL,BingLiLv REAL,ZiBanLiLv REAL,HeBanLiLv REAL,ShuangMeiLiLv REAL,HuiBanLiLv REAL,ChongShiLiLv REAL,ZiLiPingJia TEXT,DanBai REAL,ZhiFang REAL,DanZhiHe REAL,CaoGanLinKangXing TEXT,ShiZhiJianCe TEXT,HanJiYin TEXT,BoZhongPenShu INTEGER,BoZhongLiShu INTEGER,ChuMiaoShu INTEGER,ChuMiaoLiShu REAL,NaiYanXing TEXT,NaiHanXing TEXT,ShiHuaQi TEXT,ZaJiaoHuaShu INTEGER,ChengHuoJiaShu INTEGER,ZhaJiaoliShu INTEGER,ChuShuQi TEXT,WanShuQi TEXT,HuiFuLv REAL,SSRBuHeGeWeiDian TEXT,created_at TEXT NOT NULL);
DROP TABLE IF EXISTS yield_test_field_records;
CREATE TABLE yield_test_field_records (record_id INTEGER PRIMARY KEY AUTOINCREMENT,experiment_id TEXT NOT NULL,experiment_name TEXT NOT NULL,fieldid TEXT,id TEXT,user TEXT,stageid TEXT,name TEXT,ma TEXT,pa TEXT,mapa TEXT,memo TEXT,stage TEXT,next_stage TEXT,f TEXT,sele REAL,process TEXT,path TEXT,source TEXT,former_fieldid TEXT,former_stageid TEXT,code TEXT,rp INTEGER,treatment TEXT,place TEXT,rows REAL,line_number TEXT,is_ck TEXT,XiaoQuShiShouMianJi REAL,XiaoQuChanLiang REAL,HanShuiLiang REAL,MuChan REAL,BoZhongQi TEXT,ChuMiaoQi TEXT,ChuMiaoLiangFou TEXT,MiaoQiTianJianPingJia TEXT,KaiHuaQi TEXT,HuaSe TEXT,HuaQiTianJianPingJia TEXT,YeXing TEXT,RongMaoSe TEXT,ShengZhangXiXing TEXT,JieJiaXiXing TEXT,DaoFuXing TEXT,ZaoShuaiXing TEXT,ZhuXing TEXT,LuoYeXing TEXT,LieJiaXing TEXT,ChengShuQi TEXT,HuoGanChengShu TEXT,ChengShuQiTianJianPingJia TEXT,ShouHuoQi TEXT,XiaoQuShouHuoZhuShu INTEGER,ShengYuQi INTEGER,TianJianBeiZhu TEXT,HuaYeBingDuBing TEXT,NiJingDianZhongFuBing TEXT,ShuangMeiBing TEXT,HuiBanBing TEXT,XiJunXingBanDianBing TEXT,XiuBing TEXT,GenFuBing TEXT,BaoNangXianChongBing TEXT,QiTaBingHai TEXT,DouGanHeiQianYing TEXT,DouJiaMing TEXT,YaChong TEXT,ShiYeXingHaiChong TEXT,KaoZhongZhuShu INTEGER,ZhuGao REAL,DiJiaGao REAL,FenZhiShu INTEGER,ZhuJingJieShu INTEGER,JiaXing TEXT,JiaShuSe TEXT,YouXiaoJia INTEGER,WuXiaoJia INTEGER,DanZhuJiaShu INTEGER,DanZhuLiShu INTEGER,DanZhuLiZhong REAL,MeiJiaLiShu REAL,LiXing TEXT,ZhongPiSe TEXT,QiSe TEXT,ZiYeSe TEXT,ZhongPiGuangZe TEXT,BaiLiZhong REAL,WanHaoLiLv REAL,PoSuiLiLv REAL,BingLiLv REAL,ZiBanLiLv REAL,HeBanLiLv REAL,ShuangMeiLiLv REAL,HuiBanLiLv REAL,ChongShiLiLv REAL,ZiLiPingJia TEXT,DanBai REAL,ZhiFang REAL,DanZhiHe REAL,CaoGanLinKangXing TEXT,ShiZhiJianCe TEXT,HanJiYin TEXT,BoZhongPenShu INTEGER,BoZhongLiShu INTEGER,ChuMiaoShu INTEGER,ChuMiaoLiShu REAL,NaiYanXing TEXT,NaiHanXing TEXT,ShiHuaQi TEXT,ZaJiaoHuaShu INTEGER,ChengHuoJiaShu INTEGER,ZhaJiaoliShu INTEGER,ChuShuQi TEXT,WanShuQi TEXT,HuiFuLv REAL,SSRBuHeGeWeiDian TEXT,created_at TEXT NOT NULL);
DROP TABLE IF EXISTS db_meta;
CREATE TABLE db_meta (key TEXT PRIMARY KEY, value TEXT);
INSERT INTO db_meta VALUES('schema_version','2');
DROP TABLE IF EXISTS field_models;
CREATE TABLE field_models (
        field_model_id INTEGER PRIMARY KEY AUTOINCREMENT,
        field_name TEXT NOT NULL UNIQUE,
        field_len REAL,
        no_plant TEXT,
        field_layout TEXT,
        strip_width TEXT,
        protect_strip TEXT,
        cross_path_width REAL,
        row_gap REAL,
        group_rows INTEGER,
        plant_start_pos TEXT,
        plant_end_pos TEXT,
        plant_start_row INTEGER,
        plant_start_col INTEGER,
        plant_end_col INTEGER,
        plan_left INTEGER,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      );
INSERT INTO field_models VALUES(1,'常规地块1',200.0,'','w/8/w','10,6/3,10','',1.0,0.5,1,'1,1','',NULL,NULL,NULL,1,'2026-05-11 08:03:31','2026-05-11 08:03:31');
DROP TABLE IF EXISTS experiments;
CREATE TABLE experiments (
        experiment_id TEXT PRIMARY KEY,
        experiment_name TEXT NOT NULL,
        total_rows REAL,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      );
DROP TABLE IF EXISTS experiment_plant_runs;
CREATE TABLE experiment_plant_runs (
        run_id INTEGER PRIMARY KEY AUTOINCREMENT,
        experiment_id TEXT NOT NULL,
        plant_table_name TEXT NOT NULL,
        sow_table_name TEXT,
        plan_id TEXT,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        FOREIGN KEY(experiment_id) REFERENCES experiments(experiment_id) ON DELETE CASCADE,
        UNIQUE(experiment_id, plant_table_name)
      );
DROP TABLE IF EXISTS experiment_records;
CREATE TABLE experiment_records (
        record_id INTEGER PRIMARY KEY AUTOINCREMENT,
        experiment_id TEXT NOT NULL,
        fieldid TEXT,
        id TEXT,
        stageid TEXT,
        name TEXT,
        former_fieldid TEXT,
        former_stageid TEXT,
        source TEXT,
        code TEXT,
        rp TEXT,
        rows REAL,
        line_number TEXT,
        created_at TEXT NOT NULL,
        FOREIGN KEY(experiment_id) REFERENCES experiments(experiment_id) ON DELETE CASCADE
      );
DROP TABLE IF EXISTS plan_runs;
CREATE TABLE plan_runs (
        plan_id TEXT PRIMARY KEY,
        experiment_name TEXT NOT NULL,
        source_param_file TEXT,
        field_length REAL,
        field_layout TEXT,
        bridge_layout TEXT,
        row_gap REAL,
        group_rows INTEGER,
        design_from_left INTEGER,
        created_at TEXT NOT NULL
      );
DROP TABLE IF EXISTS plan_slots;
CREATE TABLE plan_slots (
        slot_id INTEGER PRIMARY KEY AUTOINCREMENT,
        plan_id TEXT NOT NULL,
        seq_no INTEGER NOT NULL,
        field_row_index INTEGER NOT NULL,
        field_row_no INTEGER,
        field_col_no INTEGER NOT NULL,
        row_length REAL,
        total_length REAL,
        interval_width REAL,
        created_at TEXT NOT NULL,
        FOREIGN KEY(plan_id) REFERENCES plan_runs(plan_id) ON DELETE CASCADE,
        UNIQUE(plan_id, seq_no)
      );
DROP TABLE IF EXISTS plant_assignments;
CREATE TABLE plant_assignments (
        assignment_id INTEGER PRIMARY KEY AUTOINCREMENT,
        plan_id TEXT NOT NULL,
        seq_no INTEGER NOT NULL,
        experiment_name TEXT NOT NULL,
        material_name TEXT NOT NULL,
        material_subrow_no INTEGER,
        field_row_no INTEGER,
        field_col_no INTEGER,
        created_at TEXT NOT NULL,
        FOREIGN KEY(plan_id) REFERENCES plan_runs(plan_id) ON DELETE CASCADE,
        UNIQUE(plan_id, seq_no)
      );
INSERT INTO sqlite_sequence VALUES('field_models',1);
INSERT INTO sqlite_sequence VALUES('field_models',1);
CREATE INDEX idx_pop_exp ON population_records(experiment_id);
CREATE INDEX idx_line_exp ON line_selection_records(experiment_id);
CREATE INDEX idx_yield_exp ON yield_test_records(experiment_id);
CREATE INDEX idx_pop_mat_exp ON population_materials(experiment_id);
CREATE INDEX idx_line_mat_exp ON line_selection_materials(experiment_id);
CREATE INDEX idx_yield_mat_exp ON yield_test_materials(experiment_id);
CREATE INDEX idx_unified_exp_id ON unified_records(experiment_id);
CREATE INDEX idx_unified_type ON unified_records(experiment_type);
CREATE INDEX idx_unified_mat_exp ON unified_materials(experiment_id);
CREATE INDEX idx_traits_exp ON traits_survey(experiment_id);
CREATE INDEX idx_pop_field_exp ON population_field_records(experiment_id);
CREATE INDEX idx_line_field_exp ON line_selection_field_records(experiment_id);
CREATE INDEX idx_yield_field_exp ON yield_test_field_records(experiment_id);
CREATE INDEX idx_plan_slots_plan_row_col ON plan_slots(plan_id, field_row_no, field_col_no);
CREATE INDEX idx_assignments_plan_material ON plant_assignments(plan_id, material_name);
CREATE INDEX idx_field_models_name ON field_models(field_name);
CREATE INDEX idx_experiments_name ON experiments(experiment_name);
CREATE INDEX idx_experiment_records_expid ON experiment_records(experiment_id);
CREATE INDEX idx_experiment_plant_runs_expid ON experiment_plant_runs(experiment_id);
COMMIT;
