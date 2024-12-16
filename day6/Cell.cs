using System;

public class Cell
{
	private bool visited = false;
	private bool isObstacle;
    public Cell(bool isObstacle) => this.isObstacle = isObstacle;

	public void Visit() {  visited = true; }
	public bool IsVisited() {  return visited; }

	public bool IsObstacle() { return isObstacle; }

}
