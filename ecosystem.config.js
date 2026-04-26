module.exports = {
  apps: [{
    name: "field-book",
    script: "./run.sh",
    cwd: "/Users/m4/Documents/GitHub/book_plot",
    interpreter: "/bin/bash",
    env: {
      PORT: 3681
    },
    autorestart: true,
    watch: false,
    max_memory_restart: "1G"
  }]
}
