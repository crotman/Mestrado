using Roots
using DataFrames
using CSV


cfo = 0
cfall = 0



print("0")

fluxos = Base.vec(collect(Base.product(collect(-0000000:-100000:-200000000),collect(0000000:100000:150000000))))
max_tamanho = 100000


tvmnpv(i,cfo,cfall)=
begin
         n=collect(1:length(cfall));
         cfo + sum(cfall./(1+i).^n)
end

f(x)=tvmnpv(x, cfo, cfall)


i_df_atual = 1


fluxos_usina = Array{Float64,1}(max_tamanho)
fluxos_anuais = Array{Float64,1}(max_tamanho)
resultados = Array{Float64,1}(max_tamanho)

n_arquivo = 1

for i = 1:length(fluxos)
         cfo = fluxos[i][1]
         cfall=repeat([fluxos[i][2]],inner=30)

         if (mod(i,10000) == 0)
                  print(i/length(fluxos))
                  print("\n")
         end

         if i_df_atual < max_tamanho

                  fluxos_usina[i_df_atual] = fluxos[i][1]
                  fluxos_anuais[i_df_atual] = fluxos[i][2]
                  resultados[i_df_atual] = try fzero(f, [0.0,1.0]) catch -1 end
         else

                  saida = DataFrame(usina = fluxos_usina, anuais = fluxos_anuais, tir = resultados)
                  CSV.write("c:\\temp\\tirs_" * string(n_arquivo)  * ".csv", saida)


                  tamanho_prox = min(max_tamanho, length(fluxos) - i )
                  fluxos_usina = Array{Float64,1}(max_tamanho)
                  fluxos_anuais = Array{Float64,1}(max_tamanho)
                  resultados = Array{Float64,1}(max_tamanho)
                  i_df_atual = 1
                  fluxos_usina[i_df_atual] = fluxos[i][1]
                  fluxos_anuais[i_df_atual] = fluxos[i][2]
                  resultados[i_df_atual] = try fzero(f, [0.0,1.0]) catch -1 end

                  n_arquivo = n_arquivo + 1
         end
         i_df_atual = i_df_atual + 1
end

saida = DataFrame(usina = fluxos_usina, anuais = fluxos_anuais, tir = resultados)
CSV.write("c:\\temp\\tirs_" * string(n_arquivo)  * ".csv", saida)
