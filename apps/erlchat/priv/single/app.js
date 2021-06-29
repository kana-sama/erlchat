const $ = x => document.querySelector(x);
$("#unlogin").addEventListener("click", () => location.reload());
$("#chat_page").style.display = "none";
$("#name").addEventListener("input", () => $("#login_submit").disabled = $("#name").value.trim() == "");
$("#message").addEventListener("input", () => $("#send_submit").disabled = $("#message").value.trim() == "");
$("#login_form").addEventListener("submit", event => {
  event.preventDefault();
  $("#username").innerText = $("#name").value;
  const ws = new WebSocket(`ws://localhost:8080/ws?name=${$("#name").value}`);
  $("#login_page").style.display = "none";
  $("#chat_page").style.display = "block";
  ws.addEventListener("message", message => {
    const data = JSON.parse(message.data);
    if ("leave" in data) $("#chat").value += `${data.leave} left the chat\n`;
    if ("enter" in data) $("#chat").value += `${data.enter} enter the chat\n`;
    if ("message" in data) $("#chat").value += `[${data.from}]: ${data.message}\n`;
  });
  ws.addEventListener("close", () => location.reload());
  $("#send_form").addEventListener("submit", event => {
    event.preventDefault();
    ws.send($("#message").value);
    $("#message").value = "";
  });
});