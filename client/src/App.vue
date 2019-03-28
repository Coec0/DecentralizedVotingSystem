<!-- 
	Här börjar det roliga

	Varje del av hemsidan består i grunden av denna filen. Vue har en träd-struktur, och denna fil är root-filen.

	<template>:
	Delen som kommer först i filen är själva HTML koden som används i denna vy (alla .vue i src/views filer är en vy).
	Notera att root aldrig kan ändras i vår Vue app, alltså kommer <NotificationHeader> och <Navigation> alltid att finnas med, oavsett var på hemsidan vi befinner oss.
	
	<router-view />:
	Används för att säga 'Här ska du sätta in .vue fil som ligger under mig i hierarkin'. Hierarkin bestäms i router.js filen.
	I router.js kan man se att om man är på sidan 'localhost:8000/' så är component = '@/views/Home' (@ refererar till src/ mappen).
	Om man dock är på 'localhost:8000/vote/:id' där :id är ett godtyckligt värde (ex: localhost:8000/vote/12345)
	så ska '@/views/Vote' användas.

	<script>:
	Varje .vue fil måste ha export default { ... } i sin script tag. Detta definierar hur .vue filen fungerar.

	name: Definierar namnet på vyn
	components: Här sätter man in alla objekt som denna vy direkt använder sig av. Se i html-delen hur vi har både NotificationHeader och Navigation taggar placerade.
		NOTERA!! Som du ser måste man också importera in dessa objekt högst upp i <script> taggen
	data(): En metod som ger tillbaka ett objekt med variabler. Dessa variabler är den data som denna vy håller koll på.
	mounted(): En metod som körs när denna vy laddas in.

	v-if="...":
	I <NotificationHeader> ser ni ett property som heter v-if="..."
	Detta property bestämmer om denna NotificationHeader ska visas eller inte.
	Det som står innanför "..." är alltså javascript-kod.
	"this" refererar alltid till hela Vue-appen.
	Om det som står innanför "..." är sant så visas NotificationHeadern på hemsidan.
	I detta fall kollar jag om det finns några notifications. 
	Om det inte finns några är .length = 0 och 0 == false i javascript. Finns det några är .length > 0 vilket är true i javascript.

	v-bind:items="votes":
	Här säger vi att vi bindar propertyn 'items' i Navigation-komponenten (mer om komponenter i components/NotificationHeader.vue)
	till votes-variabeln i denna vyn. Detta innebär att om vi ändrar 'votes' i denna vyn så uppdateras Navigation-komponenten automatisk.

	Det som är så bra med Vue är att allt är reaktivt. Detta innebär att om till exempel 'this.$store.state.notifications.length' skulle ändra på sig efter ett tag och gå från 0 -> 1
	så uppdateras detta automatisk och NotificationHeader skulle då visas.

-->

<template>
	<div id="app">
		<NotificationHeader v-if="this.$store.state.notifications.length"></NotificationHeader>
		<Navigation v-bind:items="votes"></Navigation>
		<router-view />
	</div>
</template>

<script>
import NotificationHeader from '@/components/NotificationHeader.vue';
import Navigation from '@/components/Navigation/Navigation.vue';
const Web3 = require('web3');	// Här importerar vi web3 som vi installerat genom 'npm install web3'

export default {
	name: 'App',
	components: {
		Navigation,
		NotificationHeader
	},
	data() {
		return {
			votes: null
		}
	},
	mounted() {
		// Hämta vilken elections som finns och lägg dem i vår 'votes' variabel som skapades i data()-metoden.
		this.$http.get('/getElections').then(result => {
			this.votes = result.data;
		});

		// Kolla om Web3 hittar MetaMask
		if (Web3.givenProvider) {
			this.$store.commit('ADD_NOTIFICATION', { message: 'MetaMask detected! (Ignored)', type: 'notify' });
		}
	}
};
</script>

<!--
	Här är css-delen, väldigt straight-forward om man kan css.
	Global CSS!
-->
<style>
#app {
	font-family: 'Avenir', Helvetica, Arial, sans-serif;
	-webkit-font-smoothing: antialiased;
	-moz-osx-font-smoothing: grayscale;
	text-align: center;
	background-color: #fafafa;
	min-height: 100vh;
}

.white {
	background-color: #fff;
}

.gray {
	background-color: #fafafa;
}

body {
	margin: 0px;
}

a {
	color: inherit;
}

</style>
