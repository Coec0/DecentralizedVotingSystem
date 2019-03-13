<template>
	<div class="container-fluid d-flex gray">
		<div>
			<router-link class="logo hvr-fade" to="/" tag="div">
				<span class="fas fa-home"></span>
			</router-link>
		</div>
		<NavButton class="nav-button hvr-fade" v-for="item in items" :key="item.id" :text="item.name" :to="'/vote/' + item.id.toString()"></NavButton>
	</div>
</template>

<script>
import NavButton from '@/components/Navigation/NavButton.vue';
export default {
	name: 'Navigation',
	components: {
		NavButton
	},
	props: {
		items: Array
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.logo {
	height: 50px;
	width: 75px;
	display: flex;
	justify-content: center;
	align-items: center;
}

.container-fluid {
	padding: 0px;
	border-bottom: 1px solid rgba(0, 0, 0, 0.1);
}

.nav-button {
	border-left: 1px solid rgba(0, 0, 0, 0.1);
	width: 150px;
}

.nav-button:last-child {
	border-right: 1px solid rgba(0, 0, 0, 0.1);
}

.hvr-fade {
	cursor: pointer;
	vertical-align: middle;
	-webkit-transform: perspective(1px) translateZ(0);
	transform: perspective(1px) translateZ(0);
	box-shadow: 0 0 1px rgba(0, 0, 0, 0);
	overflow: hidden;
	-webkit-transition-duration: 0.3s;
	transition-duration: 0.3s;
	-webkit-transition-property: color, background-color;
	transition-property: color, background-color;
}

.hvr-fade:hover, .hvr-fade:focus, .hvr-fade:active {
	background-color: #2c3e50;
	color: white;
}
</style>
