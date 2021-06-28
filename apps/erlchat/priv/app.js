const { useState, useMemo, useEffect, useCallback } = React;

const Block = styled.div`
  padding: 10px;
  margin: 10px;
  border: 1px solid black;
`;

function App() {
  const [chats, setChats] = useState([]);
  
  function addNewChat(name) {
    setChats(chats => [...chats, name]);
  }

  function closeChat(chat) {
    setChats(chats => chats.filter(chat_ => chat_ != chat));
  }

  return (
    <div>
      {chats.map(chat => <Block key={chat}>{chat}<Chat chat={chat} onLeave={() => closeChat(chat)} /></Block>)}
      <Block>
        New chat:
        <InputForm onSubmit={addNewChat} />
      </Block>
    </div>
  )
}

function Chat({ chat, onLeave }) {
  const ws = useMemo(() => new WebSocket(`ws://localhost:8080/ws?name=${chat}`), []);
  const [messages, setMessages] = useState([]);
  const [members, setMembers] = useState([]);

  useEffect(() => {
    ws.addEventListener("message", message => {
      const data = JSON.parse(message.data);
      if ("members" in data)
        setMembers(data.members);
      if ("leave" in data) {
        setMessages(messages => [...messages, `${data.leave} left chat`]);
        setMembers(members => members.filter(member => member != data.leave));
      }
      if ("enter" in data) {
        setMessages(messages => [...messages, `${data.enter} entered chat`]);
        setMembers(members => [...members, data.enter]);
      }
      if ("message" in data)
        setMessages(messages => [...messages, `[${data.from}]: ${data.message}`]);
    });

    ws.addEventListener("close", () => {
      onLeave();
    });

    return () => ws.close();
  }, []);

  return (
    <>
      <div>Members: {members.join(", ")}</div>
      <ul>{messages.map(message => <li key={message}>{message}</li>)}</ul>
      <button onClick={onLeave}>leave chat</button>
      <InputForm onSubmit={message => ws.send(message)} />
    </>
  )
}

function InputForm({ onSubmit }) {
  const [value, setValue] = useState("");
  const isValid = value.trim() != "";
  
  const handleSubmit = useCallback(event => {
    event.preventDefault();
    if (isValid) {
      onSubmit(value);
      setValue("");
    }
  }, [value, isValid]);

  return (
    <form onSubmit={handleSubmit}>
      <input value={value} onChange={event => setValue(event.currentTarget.value)} />
      <input type="submit" value="login" disabled={!isValid} />
    </form>
  )
}

ReactDOM.render(
  <App/>,
  document.querySelector("#root")
);
