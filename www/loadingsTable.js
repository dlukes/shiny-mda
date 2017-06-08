function(row, data) {
  /* palette generated in R with:
     library(pals)
     warmcool(length(seq(-1, 1, .1)))
  */
  var palette = [
    "#B40426", "#C53033", "#D55243", "#E36C55", "#ED8467", "#F3987A", "#F6AB8D", "#F6BBA2",
    "#F0C9B5", "#E8D4C9", "#DDDDDD", "#CFD8E9", "#BFD2F3", "#AFC9FB", "#9EBDFE", "#8CAEFC",
    "#7B9DF7", "#698AEF", "#5876E1", "#4961D1", "#3B4CC0"
  ];

  var cells = row.children;
  cells[0].classList.add("dt-right");
  for (var i = 0; i < data.length; i++) {
    var value = data[i];
    if (/^[\d\.\-]+$/.test(value)) {
      var round = value < 0 ? Math.floor : Math.ceil;
      var colorIdx = round((value + 1) / 2 * 20);
      var cell = cells[i];
      cell.style.background = palette[colorIdx];
      cell.style.fontWeight = "bold";
      if (Math.abs(value) > 0.6) {
        cell.style.color = "white";
      }
      cell.innerHTML = value.toFixed(2);
    }
  }
}
