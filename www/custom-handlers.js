// 等待 Shiny 初始化后再添加 handlers
$(document).ready(function() {

  Shiny.addCustomMessageHandler('auto_download_when_ready', function(message) {
    if (!message || !message.id) {
      return;
    }

    var attempts = 0;
    var maxAttempts = message.maxAttempts || 40;
    var intervalMs = message.intervalMs || 250;

    var timer = window.setInterval(function() {
      var el = document.getElementById(message.id);
      if (!el) {
        attempts += 1;
      } else {
        var href = el.getAttribute('href') || '';
        var disabled = el.classList.contains('disabled') || el.getAttribute('aria-disabled') === 'true';

        if (href && !disabled) {
          window.clearInterval(timer);
          el.click();
          return;
        }
        attempts += 1;
      }

      if (attempts >= maxAttempts) {
        window.clearInterval(timer);
        if (window.Shiny && message.failInputId) {
          window.Shiny.setInputValue(message.failInputId, Date.now(), { priority: 'event' });
        }
      }
    }, intervalMs);
  });

  Shiny.addCustomMessageHandler('toggle_delete_btn', function(message) {
    var sel = '[id$="btn_delete_exp"]';
    var btns = document.querySelectorAll(sel);
    btns.forEach(function(btn) {
      btn.style.display = message.show ? 'inline-block' : 'none';
    });
  });

  // 试验列表点击处理
  $(document).on('click', '.exp-item', function() {
    var expId = $(this).attr('data-exp-id');
    if (expId) {
      // 移除其他选中状态
      $('.exp-item').removeClass('selected');
      $(this).addClass('selected');
      // 向上查找包含 module id 的容器
      var container = $(this).closest('[id]');
      var containerId = '';
      container.parents('[id]').each(function() {
        var id = $(this).attr('id');
        if (id && id.indexOf('experiments') !== -1) {
          containerId = id;
          return false;
        }
      });
      if (!containerId) containerId = container.attr('id') || '';
      // 构造正确的 input id (将 - 转为 _)
      var baseId = containerId.replace(/-/g, '_');
      var inputId = baseId + '_experiment_list_click';
      // 设置 Shiny input value
      if (window.Shiny) {
        Shiny.setInputValue(inputId, expId, { priority: 'event' });
      }
    }
  });

});
