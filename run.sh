#!/bin/bash
# R Shiny App 启动脚本
# 用法: ./run.sh
# 端口: 3636
# 访问: http://<MacMini的IP>:3636

# 获取脚本所在目录
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# 端口
PORT=3681

# 显示信息
echo "=========================================="
echo "田间记录本生成器"
echo "=========================================="
echo "工作目录: $SCRIPT_DIR"
echo "端口: $PORT"
echo "局域网访问: http://<本机IP>:$PORT"
echo "本地访问: http://localhost:$PORT"
echo "=========================================="

# 获取本机 IP 地址（局域网）
if [[ "$(uname)" == "Darwin" ]]; then
    # macOS
    IP=$(ipconfig getifaddr en0 2>/dev/null || ipconfig getifaddr en1 2>/dev/null || echo "请手动查看")
    echo "本机 IP: $IP"
fi

echo ""
echo "启动 R Shiny 应用..."
echo "按 Ctrl+C 停止"
echo "=========================================="

# 启动 Shiny 应用
# host = "0.0.0.0" 允许局域网访问
exec Rscript -e "\
options(shiny.host = '0.0.0.0'); \
shiny::runApp(\
  appDir = '.', \
  port = $PORT, \
  host = '0.0.0.0', \
  launch.browser = FALSE \
)"