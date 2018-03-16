function kontextSearch() {
  var chunk_id = document.querySelector("#chunk_id").textContent;
  var within;
  if (chunk_id === "NA") {
    var modes = [];
    document.querySelectorAll("#mode input").forEach(function(i) {
      if (i.checked) {
        modes.push(i.value);
      }
    });
    var divisions = [];
    document.querySelectorAll("#division input").forEach(function(i) {
      if (i.checked) {
        divisions.push(i.value.split(/\s+/)[1]);
      }
    });
    modes = modes.join("|");
    divisions = divisions.join("|");
    within = ' within <chunk mode="' + modes + '" | division="' + divisions + '"/>';
  } else {
    within = ' within <chunk id="' + chunk_id + '"/>';
  }
  var corpname = document.getElementById("click_info").textContent.includes("cpact") ? "cpact" : "koditex";
  var cql = document.querySelector("#cql").value + within;
  var url = "https://kontext.korpus.cz/first?corpname=" + corpname +
    "&queryselector=cqlrow&default_attr=word&cql=" + encodeURIComponent(cql);
  console.log(url);
  window.open(url, "_blank");
}
