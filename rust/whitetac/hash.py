city_names = [
	"Adenarith",
	"Amsterdam",
	"Anápolis",
	"Aparecida de Goiânia",
	"Athens",
	"Austin",
	"Bahrain",
	"Bangalore",
	"Barcelona",
	"Belo Horizonte",
	"Belém",
	"Boa Vista",
	"Boston",
	"Brasília",
	"Brussels",
	"Bucharest",
	"Campinas",
	"Canada",
	"Central",
	"Chennai",
	"Chongqing",
	"Copenhagen",
	"Cuiabá",
	"Curitiba",
	"Dallas",
	"Dublin",
	"Duque de Caxias",
	"Feira de Santana",
	"Fortaleza",
	"Frankfurt",
	"Gaaphis",
	"Goiania",
	"Guadalajara",
	"Guarulhos",
	"Helsinki",
	"Hong Kong",
	"Hyderabad",
	"Indianapolis",
	"Ireland",
	"Istanbul",
	"Juiz de Fora",
	"Kiev",
	"Kolkata",
	"Krofast",
	"Krore",
	"Larfast",
	"London",
	"Londrina",
	"Los Angeles",
	"Macapá",
	"Madrid",
	"Manaus",
	"Mexico City",
	"Miami",
	"Milan",
	"Montreal",
	"Moscow",
	"Mumbai",
	"N. California",
	"N. Virginia",
	"New Delhi",
	"New York",
	"Niterói",
	"Nova Iguaçu",
	"Ohio",
	"Oregon",
	"Osaka",
	"Osasco",
	"Oslo",
	"Palmas",
	"Paris",
	"Porto Alegre",
	"Porto Velho",
	"Prico",
	"Prover",
	"Pune",
	"Qreigh",
	"Qrokwood",
	"Recife",
	"Ribeirão Preto",
	"Rio de Janeiro",
	"Salvador",
	"Santo André",
	"Sao Paulo",
	"Seoul",
	"Singapore",
	"St. Petersburg",
	"Stockholm",
	"Sydney",
	"São Bernardo do Campo",
	"São Gonçalo",
	"São José dos Campos",
	"São Paulo",
	"Tokyo",
	"Toronto",
	"Urgtin",
	"Vancouver",
	"Vienna",
	"Warsaw",
	"Zurich",
]
# print("let city_names: Vec<String> = vec![");
# for i in range(len(city_names)):
#     l = city_names[i]
#     l = "\"" + l + "\""+".to_string(),"
#     print(l)
# print("];")

# for i in range(len(city_names)):
#     l = str(list(city_names[i].encode('utf-8')))
#     l = "vec!" + l + ""
#     print("builder.push("  + l + ","+str(i)+");")

hashs = []
for i in range(len(city_names)):
    l = list(city_names[i].encode('utf-8'))
    s = 0
    for j in range(7):
        if(j >= len(l)):
            break
        s += l[j] << (j)*8
    if(s in hashs):
        print("Hash collision")
        print(s)
        print(city_names[i])
        print(city_names[hashs.index(s)])
        import sys
        sys.exit(1)
    if(s >= 2**63):
        print("Hash too big")
        print(city_names[i])
        import sys
        sys.exit(1)
    hashs.append(s)

for i in range(len(hashs)):
    print(str(hashs[i]) + "u64 => \"" + city_names[i] + "\",")