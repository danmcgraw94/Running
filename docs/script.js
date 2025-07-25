const tableBody = document.getElementById("table-body");

trainingData.forEach(row => {
  const tr = document.createElement("tr");
  tr.innerHTML = `
    <td>${row.date}</td>
    <td>${row.day}</td>
    <td>${row.workout}</td>
    <td>${row.miles}</td>
  `;
  tableBody.appendChild(tr);
});
