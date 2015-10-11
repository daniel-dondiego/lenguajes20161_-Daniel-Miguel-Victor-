public class Ejercicio4{
		
	static boolean foo(boolean n){
		return (true && n); //función que depende de n es decir siempre devuelve n
	}

	public static void main(String[] args) {
		boolean b = true; //inicializamos b

		boolean resp = foo(b); // b = true pero como foo se ejecutó resp = true

		b = false; //b cambia de valor 

		boolean eval = true && resp; // se calcula la expresión, si java fuera perezoso ésto cambiaría el sentido de la evaluación ya que tomaría el valor false de b

		System.out.println(eval); //finalmente vemos que toma el valor que alcanza cuando se ejecutó y así resp ya no contiene a la función sino al resultado, lo que nos da consistencia
								//en el programa, por lo que podemos mostrar que si java fuera perezoso haría incongruente al programa al tomar el ultimo valor
								//pero este se ejecutó cuando queríamos así que por esa razón no se permite el cambio de variables en un lenguaje perezoso
	}						  
}