function kontextSearch() {
  var chunk_id = document.querySelector("#chunk_id").textContent;
  var cql = document.querySelector("#cql").value + ' within <chunk id="' + chunk_id + '"/>';
  var url = "https://kontext.korpus.cz/first?corpname=koditex&queryselector=cqlrow&default_attr=word&cql=" + encodeURIComponent(cql);
  window.open(url, "_blank");
}