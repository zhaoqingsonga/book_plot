# =============================================================================
# 共享字段常量 — Single Source of Truth
# 被 import_traits.R、import_regional.R、mod_regional_import.R 等模块共享
# 使用方式：source(file.path(getwd(), "shared/field_constants.R"), local = TRUE)
# =============================================================================

# ----- 基础字段（不写入性状表的核心字段）-----
BASE_FIELDS <- c(
  "experiment_id", "experiment_name",
  "fieldid", "id", "user", "stageid", "name", "ma", "pa", "mapa", "memo",
  "stage", "next_stage", "f", "sele", "process", "path", "source",
  "former_fieldid", "former_stageid", "code", "rp", "treatment", "place",
  "rows", "line_number", "is_ck", "created_at",
  "import_batch_id", "import_time", "trial_name", "group_label"
)

# ----- 性状字段列表（田试记录表中的可更新字段）-----
# 派生自 FIELD_RECORD_COLS，扣除 BASE_FIELDS
TRAIT_FIELD_NAMES <- c(
  "XiaoQuShiShouMianJi", "XiaoQuChanLiang", "HanShuiLiang", "MuChan",
  "BoZhongQi", "ChuMiaoQi", "ChuMiaoLiangFou", "MiaoQiTianJianPingJia",
  "KaiHuaQi", "HuaSe", "HuaQiTianJianPingJia", "YeXing", "RongMaoSe",
  "ShengZhangXiXing", "JieJiaXiXing", "DaoFuXing", "ZaoShuaiXing", "ZhuXing",
  "LuoYeXing", "LieJiaXing", "ChengShuQi", "HuoGanChengShu", "ChengShuQiTianJianPingJia",
  "ShouHuoQi", "XiaoQuShouHuoZhuShu", "ShengYuQi", "TianJianBeiZhu",
  "HuaYeBingDuBing", "NiJingDianZhongFuBing", "ShuangMeiBing", "HuiBanBing",
  "XiJunXingBanDianBing", "XiuBing", "GenFuBing", "BaoNangXianChongBing",
  "QiTaBingHai", "DouGanHeiQianYing", "DouJiaMing", "YaChong", "ShiYeXingHaiChong",
  "KaoZhongZhuShu", "ZhuGao", "DiJiaGao", "FenZhiShu", "ZhuJingJieShu", "JiaXing",
  "JiaShuSe", "YouXiaoJia", "WuXiaoJia", "DanZhuJiaShu", "DanZhuLiShu", "DanZhuLiZhong",
  "MeiJiaLiShu", "LiXing", "ZhongPiSe", "QiSe", "ZiYeSe", "ZhongPiGuangZe",
  "BaiLiZhong", "WanHaoLiLv", "PoSuiLiLv", "BingLiLv", "ZiBanLiLv", "HeBanLiLv",
  "ShuangMeiLiLv", "HuiBanLiLv", "ChongShiLiLv", "ZiLiPingJia",
  "DanBai", "ZhiFang", "DanZhiHe", "CaoGanLinKangXing", "ShiZhiJianCe", "HanJiYin",
  "BoZhongPenShu", "BoZhongLiShu", "ChuMiaoShu", "ChuMiaoLiShu",
  "NaiYanXing", "NaiHanXing", "ShiHuaQi", "ZaJiaoHuaShu", "ChengHuoJiaShu", "ZhaJiaoliShu",
  "ChuShuQi", "WanShuQi", "HuiFuLv", "SSRBuHeGeWeiDian"
)

# ----- 数值型性状字段 -----
NUMERIC_TRAIT_FIELDS <- c(
  "MuChan", "XiaoQuChanLiang", "XiaoQuShiShouMianJi",
  "ShengYuQi", "ZhuGao", "BaiLiZhong", "DanBai", "ZhiFang", "HanShuiLiang",
  "KaoZhongZhuShu", "XiaoQuShouHuoZhuShu", "DiJiaGao", "FenZhiShu",
  "ZhuJingJieShu", "YouXiaoJia", "WuXiaoJia", "DanZhuJiaShu", "DanZhuLiShu",
  "DanZhuLiZhong", "MeiJiaLiShu", "WanHaoLiLv", "PoSuiLiLv",
  "BingLiLv", "ZiBanLiLv", "HeBanLiLv", "ShuangMeiLiLv", "HuiBanLiLv",
  "ChongShiLiLv", "DanZhiHe", "BoZhongPenShu", "BoZhongLiShu",
  "ChuMiaoShu", "ChuMiaoLiShu", "ZaJiaoHuaShu", "ChengHuoJiaShu",
  "ZhaJiaoliShu", "HuiFuLv"
)

# ----- 质量性状字段（soyplant::qr_trait$field_type == "C"）-----
QUALITY_TRAIT_FIELDS <- c(
  "HuaSe", "YeXing", "RongMaoSe", "JieJiaXiXing", "DaoFuXing",
  "ShengZhangXiXing", "ZhuXing", "LuoYeXing", "LieJiaXing",
  "JiaXing", "JiaShuSe", "LiXing", "ZhongPiSe", "QiSe", "ZiYeSe",
  "ZhongPiGuangZe", "ChuMiaoLiangFou", "MiaoQiTianJianPingJia",
  "HuaQiTianJianPingJia", "ChengShuQiTianJianPingJia", "ZiLiPingJia",
  "HuoGanChengShu", "ZaoShuaiXing", "CaoGanLinKangXing"
)

# ----- 日期型性状字段（可能以 Excel 序列数存储）-----
DATE_TRAIT_FIELDS <- c(
  "BoZhongQi", "ChuMiaoQi", "KaiHuaQi", "ChengShuQi", "ChengShuQiTianJianPingJia",
  "ShouHuoQi", "ShiHuaQi", "ChuShuQi", "WanShuQi"
)

# ----- 字段中文显示名映射 -----
# 格式: "字段名" = "中文名"
TRAIT_DISPLAY_NAMES <- c(
  "田间标识号(fieldid)"          = "fieldid",
  "品种(name)"                  = "name",
  "编号(code)"                  = "code",
  "地点(place)"                 = "place",
  "亩产(MuChan)"                = "MuChan",
  "小区产量"                    = "XiaoQuChanLiang",
  "小区实收面积"                 = "XiaoQuShiShouMianJi",
  "含水量"                      = "HanShuiLiang",
  "序号(stageid)"               = "stageid",
  "母本(ma)"                    = "ma",
  "父本(pa)"                    = "pa",
  "重复(rp)"                    = "rp",
  "行数(rows)"                  = "rows",
  "行号(line_number)"           = "line_number",
  "对照类型(is_ck)"             = "is_ck",
  "播种期"                      = "BoZhongQi",
  "出苗期"                      = "ChuMiaoQi",
  "出苗良否"                    = "ChuMiaoLiangFou",
  "苗期田间评价"                 = "MiaoQiTianJianPingJia",
  "开花期"                      = "KaiHuaQi",
  "花色(HuaSe)"                 = "HuaSe",
  "花期田间评价"                 = "HuaQiTianJianPingJia",
  "叶形(YeXing)"                = "YeXing",
  "茸毛色(RongMaoSe)"            = "RongMaoSe",
  "生长习性"                    = "ShengZhangXiXing",
  "结荚习性"                    = "JieJiaXiXing",
  "倒伏性(DaoFuXing)"            = "DaoFuXing",
  "早衰性"                      = "ZaoShuaiXing",
  "株型(ZhuXing)"               = "ZhuXing",
  "落叶性"                      = "LuoYeXing",
  "裂荚性"                      = "LieJiaXing",
  "成熟期"                      = "ChengShuQi",
  "活秆成熟"                    = "HuoGanChengShu",
  "成熟期田间评价"               = "ChengShuQiTianJianPingJia",
  "收获期"                      = "ShouHuoQi",
  "小区收获株数"                 = "XiaoQuShouHuoZhuShu",
  "生育期(ShengYuQi)"            = "ShengYuQi",
  "田间备注"                     = "TianJianBeiZhu",
  "花叶病毒病"                   = "HuaYeBingDuBing",
  "拟茎点种腐病"                 = "NiJingDianZhongFuBing",
  "霜霉病"                      = "ShuangMeiBing",
  "灰斑病"                      = "HuiBanBing",
  "细菌性斑点病"                 = "XiJunXingBanDianBing",
  "锈病"                        = "XiuBing",
  "根腐病"                      = "GenFuBing",
  "孢囊线虫病"                   = "BaoNangXianChongBing",
  "其他病害"                     = "QiTaBingHai",
  "豆秆黑潜蝇"                   = "DouGanHeiQianYing",
  "豆荚螟"                      = "DouJiaMing",
  "蚜虫"                        = "YaChong",
  "食叶性害虫"                   = "ShiYeXingHaiChong",
  "考种株数"                     = "KaoZhongZhuShu",
  "株高(ZhuGao)"                = "ZhuGao",
  "底荚高(DiJiaGao)"            = "DiJiaGao",
  "分枝数"                      = "FenZhiShu",
  "主茎节数"                     = "ZhuJingJieShu",
  "荚形(JiaXing)"               = "JiaXing",
  "荚熟色"                      = "JiaShuSe",
  "有效荚"                      = "YouXiaoJia",
  "无效荚"                      = "WuXiaoJia",
  "单株荚数"                     = "DanZhuJiaShu",
  "单株粒数"                     = "DanZhuLiShu",
  "单株粒重"                     = "DanZhuLiZhong",
  "每荚粒数"                     = "MeiJiaLiShu",
  "粒型(LiXing)"                = "LiXing",
  "种皮色(ZhongPiSe)"            = "ZhongPiSe",
  "脐色(QiSe)"                  = "QiSe",
  "子叶色"                      = "ZiYeSe",
  "种皮光泽"                     = "ZhongPiGuangZe",
  "百粒重(BaiLiZhong)"          = "BaiLiZhong",
  "完好粒率"                     = "WanHaoLiLv",
  "破碎粒率"                     = "PoSuiLiLv",
  "病粒率"                      = "BingLiLv",
  "紫斑粒率"                     = "ZiBanLiLv",
  "褐斑粒率"                     = "HeBanLiLv",
  "霜霉粒率"                     = "ShuangMeiLiLv",
  "灰斑粒率"                     = "HuiBanLiLv",
  "虫蚀粒率"                     = "ChongShiLiLv",
  "籽粒评价"                     = "ZiLiPingJia",
  "蛋白(DanBai)"                = "DanBai",
  "脂肪(ZhiFang)"               = "ZhiFang",
  "蛋脂和"                      = "DanZhiHe",
  "草甘膦抗性"                   = "CaoGanLinKangXing",
  "试纸检测"                     = "ShiZhiJianCe",
  "含基因"                      = "HanJiYin",
  "播种盆数"                     = "BoZhongPenShu",
  "播种粒数"                     = "BoZhongLiShu",
  "出苗数"                      = "ChuMiaoShu",
  "出苗粒数"                     = "ChuMiaoLiShu",
  "耐盐性"                      = "NaiYanXing",
  "耐旱性"                      = "NaiHanXing",
  "始花期"                      = "ShiHuaQi",
  "杂交花数"                     = "ZaJiaoHuaShu",
  "成活荚数"                     = "ChengHuoJiaShu",
  "杂交粒数"                     = "ZhaJiaoliShu",
  "初熟期"                      = "ChuShuQi",
  "完熟期"                      = "WanShuQi",
  "恢复率"                      = "HuiFuLv",
  "SSR不合格"                   = "SSRBuHeGeWeiDian",
  "忽略"                        = "ignore"
)